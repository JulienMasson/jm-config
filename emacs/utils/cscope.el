;;; cscope.el --- Modern Emacs Cscope Interface

;; Copyright (C) 2019 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; URL: https://github.com/JulienMasson/jm-config/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'subr-x)
(require 'magit-git)
(require 'cscope-request)

(cl-defstruct cscope-data
  dir desc pattern regexp start)

;; mode
(defvar cscope-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'cscope-enter)
    (define-key map "c" 'cscope-cancel-current-request)
    (define-key map "C" 'cscope-cancel-all-requests)
    (define-key map "d" 'cscope-erase-request)
    (define-key map "f" 'cscope-toggle-fast-symbol)
    (define-key map "D" 'cscope-erase-all)
    (define-key map "n" 'cscope-next-file)
    (define-key map "p" 'cscope-previous-file)
    (define-key map "q" 'cscope-quit)
    (define-key map "u" 'cscope-toggle-auto-update)
    (define-key map "U" 'cscope-recreate-database)
    (define-key map (kbd "C-n") 'cscope-next-request)
    (define-key map (kbd "C-p") 'cscope-previous-request)
    map))

(define-derived-mode cscope-mode fundamental-mode
  "cscope"
  (cscope-update-header-line)
  (toggle-read-only t))

;; External vars
(defvar cscope-database-list nil)

;; Internal vars
(defvar cscope-buffer-name "*cscope*")
(defvar cscope-result-separator (concat (make-string 80 (string-to-char "="))))
(defvar cscope-file-entry "***")
(defvar cscope-index-file "cscope.files")
(defvar cscope-database-file "cscope.out")
(defvar cscope-database-fast-symbol-files '("cscope.out.in" "cscope.out.po"))
(defvar cscope-default-regexp
  "^\\(.*\\)[ \t]+\\(.*\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)")
(defvar cscope-prompt-history nil)

(defvar cscope-marker-ring-max 32)
(defvar cscope-marker-ring nil)
(defvar cscope-marker-index 0)

(defvar cscope-auto-update t)
(defvar cscope-fast-symbol nil)

;; common
(defun cscope-abort ()
  (signal 'quit t))

(defun cscope-bold-string (str pattern)
  (if (string-match pattern str)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(concat (substring str 0 beg)
		(propertize pattern 'face 'bold)
		(substring str end (length str))))
    str))

(defun cscope-get-time-seconds ()
  (string-to-number (format-time-string "%s.%3N" (current-time))))

(defun cscope-update-header-line ()
  (let* ((fmt "%s: %s  ")
	 (on (propertize "on" 'face 'success))
	 (off (propertize "off" 'face 'error))
	 (update (propertize "[auto-update]" 'face 'bold))
	 (update-value (if cscope-auto-update on off))
	 (fast (propertize "[fast-symbol]" 'face 'bold))
	 (fast-value (if cscope-fast-symbol on off)))
    (setq header-line-format (concat (format fmt update update-value)
				     (format fmt fast fast-value)))))

(defun cscope-build-default-option ()
  (append (list "-k" "-i" (eval cscope-index-file)
		"-f" (eval cscope-database-file))
	  (if cscope-fast-symbol '("-q"))))

(defun cscope-point-max ()
  (with-current-buffer cscope-buffer-name
    (point-max)))

(defun cscope-switch-to-buffer (buffer)
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)))

(defun cscope-check-env ()
  (unless (get-buffer cscope-buffer-name)
    (with-current-buffer (get-buffer-create cscope-buffer-name)
      (cscope-mode))
    (setq cscope-marker-ring (make-ring cscope-marker-ring-max))))

(defun cscope-save-marker ()
  (setq cscope-marker-index 0)
  (let ((marker (point-marker)))
    (when-let ((index (ring-member cscope-marker-ring marker)))
	(ring-remove cscope-marker-ring index))
    (ring-insert cscope-marker-ring marker)))

(defun cscope-get-next-marker ()
  (unless (ring-empty-p cscope-marker-ring)
    (let ((index cscope-marker-index)
	  (length (ring-length cscope-marker-ring)))
      (setq cscope-marker-index (mod (+ index 1) length))
      (ring-ref cscope-marker-ring cscope-marker-index))))

(defun cscope-pop-mark ()
  (interactive)
  (when-let* ((marker (cscope-get-next-marker))
	      (buffer (marker-buffer marker))
	      (pos (marker-position marker)))
    (when (get-buffer buffer)
      (cscope-switch-to-buffer buffer)
      (goto-char pos))))

;; buffer insertion
(defun cscope-insert (str)
  (with-current-buffer cscope-buffer-name
    (let ((inhibit-read-only t))
      (save-excursion
	(goto-char (point-max))
	(insert str)))))

(defun cscope-insert-separator ()
  (cscope-insert (propertize (concat cscope-result-separator "\n")
			     'face 'font-lock-comment-delimiter-face)))

(defun cscope-insert-header (dir desc)
  (cscope-insert (format "\n━▶ Default directory: %s\n"
			 (propertize dir 'face 'font-lock-keyword-face)))
  (cscope-insert (concat "\n" desc "\n")))

(defun cscope-insert-next-header (data)
  (setf (cscope-data-start data) (cscope-get-time-seconds))
  (cscope-insert-header (cscope-data-dir data) (cscope-data-desc data)))

(defun cscope-insert-initial-header (data)
  (cscope-insert-separator)
  (cscope-insert-next-header data)
  (cscope-switch-to-buffer cscope-buffer-name)
  (goto-char (point-max)))

(defun cscope-insert-request-fail (output error data)
  (cscope-insert (concat (propertize "ERROR: " 'face 'error)
			 error "\n"
			 (mapconcat 'identity output "\n") "\n")))

;; mode functions
(defun cscope-enter ()
  (interactive)
  (let ((file (get-text-property (point) 'cscope-file))
	(pattern (get-text-property (point) 'cscope-pattern))
	(line (get-text-property (point) 'cscope-line)))
    (when file
      (cscope-switch-to-buffer (find-file-noselect file))
      (when line
	(goto-line line)
	(when pattern
	  (end-of-line)
	  (search-backward pattern nil t)))
      (cscope-save-marker))))

(defun cscope-quit ()
  (interactive)
  (kill-buffer cscope-buffer-name))

(defun cscope-next-pattern (pattern)
  (with-current-buffer cscope-buffer-name
    (end-of-line)
    (search-forward pattern nil t)
    (beginning-of-line)))

(defun cscope-previous-pattern (pattern)
  (with-current-buffer cscope-buffer-name
    (beginning-of-line)
    (search-backward pattern nil t)
    (beginning-of-line)))

(defun cscope-next-file ()
  (interactive)
  (cscope-next-pattern cscope-file-entry))

(defun cscope-previous-file ()
  (interactive)
  (cscope-previous-pattern cscope-file-entry))

(defun cscope-next-request ()
  (interactive)
  (cscope-next-pattern cscope-result-separator))

(defun cscope-previous-request ()
  (interactive)
  (cscope-previous-pattern cscope-result-separator))

(defun cscope-erase-request ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
	  beg)
      (cscope-previous-request)
      (setq beg (point))
      (cscope-next-request)
      (if (eq (point) beg)
	  (delete-region beg (point-max))
	(delete-region beg (point))))))

(defun cscope-erase-all ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun cscope-toggle-auto-update ()
  (interactive)
  (setq cscope-auto-update (not cscope-auto-update))
  (cscope-update-header-line))

(defun cscope-toggle-fast-symbol ()
  (interactive)
  (setq cscope-fast-symbol (not cscope-fast-symbol))
  (mapc #'cscope-database-remove-files cscope-database-list)
  (mapc #'cscope-create-database cscope-database-list)
  (cscope-update-header-line))

(defun cscope-recreate-database ()
  (interactive)
  (mapc #'cscope-create-database cscope-database-list))

;; find management
(defun cscope-find-default-option ()
  (append (cscope-build-default-option)
	  (unless cscope-auto-update '("-d"))))

(defun cscope-jump-first-result ()
  (with-current-buffer cscope-buffer-name
    (goto-char (point-max))
    (search-backward cscope-result-separator nil t)
    (search-forward cscope-file-entry nil t)
    (next-line)
    (cscope-enter)))

(defun cscope-parse-line (line regexp &optional pattern)
  (if (functionp regexp)
      (funcall regexp line pattern)
    (when (string-match regexp line)
      (let ((file (substring line (match-beginning 1) (match-end 1)))
	    (func (substring line (match-beginning 2) (match-end 2)))
	    (line-nbr (substring line (match-beginning 3) (match-end 3)))
	    (line-str (substring line (match-beginning 4) (match-end 4))))
	(cons file `((:func ,func :line-nbr ,line-nbr :line-str ,line-str)))))))

(defun cscope-build-assoc-results (output regexp &optional pattern)
  (let (results)
    (mapc (lambda (line)
    	    (when-let ((result (cscope-parse-line line regexp pattern)))
	      (if-let ((data (assoc-default (car result) results)))
	      	  (setcdr (assoc (car result) results)
	      		  (add-to-list 'data (cadr result) t))
    		(add-to-list 'results result t))))
	  output)
    results))

(defun cscope-propertize-line (beg end file &optional pattern line)
  (with-current-buffer cscope-buffer-name
    (let ((inhibit-read-only t)
	  plist)
      (setq plist (plist-put plist 'cscope-file file))
      (when pattern
	(setq plist (plist-put plist 'cscope-pattern pattern)))
      (when line
	(setq plist (plist-put plist 'cscope-line line)))
      (add-text-properties beg (point-max) plist))))

(defun cscope-insert-line-entry (pattern func nbr str)
  (let ((func-prop (propertize func 'face 'font-lock-type-face))
	(nbr-prop (propertize nbr 'face 'font-lock-string-face))
	(str-prop (cscope-bold-string str pattern))
	(fmt-line "%-35s %s")
	(fmt-header "%s[%s]"))
    (cscope-insert (format fmt-line (format fmt-header func-prop nbr-prop)
			   str-prop))))

(defun cscope-insert-results (dir pattern results)
  (mapc (lambda (result)
	  (let* ((file (car result))
		 (data (cdr result))
		 (beg (cscope-point-max)))
	    ;; insert file
	    (cscope-insert (propertize (format "%s %s:" cscope-file-entry file)
				       'face 'font-lock-constant-face))
	    (cscope-propertize-line beg (cscope-point-max) (concat dir file))
	    (cscope-insert "\n")

	    ;; insert data
	    (mapc (lambda (elem)
	    	    (let ((func (plist-get elem :func))
	    		  (line-nbr (plist-get elem :line-nbr))
	    		  (line-str (plist-get elem :line-str))
	    		  (beg (cscope-point-max))
	    		  plist)
		      (cscope-insert-line-entry pattern func line-nbr line-str)
	    	      (cscope-propertize-line beg (cscope-point-max)
	    				      (concat dir file)
					      pattern
	    				      (string-to-number line-nbr))
	    	      (cscope-insert "\n")))
	    	  data)
	    (cscope-insert "\n")))
	results))

(defun cscope-find-finish (output error data)
  (let* ((regexp (cscope-data-regexp data))
	 (dir (cscope-data-dir data))
	 (pattern (cscope-data-pattern data))
	 (results (cscope-build-assoc-results output regexp pattern)))
    (if results
	(cscope-insert-results dir pattern results)
      (cscope-insert " --- No matches were found ---\n\n"))
    (when (and (eq (length results) 1)
	       (eq (length (cdar results)) 1))
      (cscope-jump-first-result))
    (cscope-insert (format "Search time = %.2f seconds\n\n"
			   (- (cscope-get-time-seconds)
			      (cscope-data-start data))))))

(defun cscope-create-request (dir desc cmd pattern regexp start)
  (let ((data (make-cscope-data :dir dir
				:desc desc
				:pattern pattern
				:regexp regexp)))
    (make-cscope-request :dir dir :cmd cmd
			 :start start
			 :fail 'cscope-insert-request-fail
			 :finish 'cscope-find-finish
			 :data data)))

(defun cscope-find-command (desc cmd pattern regexp)
  (cscope-check-env)
  (cscope-check-database)
  (let* ((cmd (append (cscope-find-default-option) cmd))
	 (first-request (list (cscope-create-request
			       (car cscope-database-list)
			       desc cmd pattern regexp
			       'cscope-insert-initial-header)))
	 (next-requests (mapcar (lambda (dir)
				  (cscope-create-request
				   dir desc cmd pattern regexp
				   'cscope-insert-next-header))
				(cdr cscope-database-list)))
	 (requests (append first-request next-requests)))
    (mapc #'cscope-process-request requests)))

(defun cscope-prompt-for-symbol (prompt)
  (let ((symbol (substring-no-properties
		 (symbol-name (symbol-at-point)))))
    (read-string (concat prompt " ("
			 (propertize symbol 'face 'success)
			 "): ")
		 nil 'cscope-prompt-history symbol)))

(defun cscope-find-symbol (symbol)
  (interactive (list (cscope-prompt-for-symbol "Find symbol")))
  (let* ((desc (format "Finding symbol: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd `("-L" "-0" ,symbol))
	 (regexp cscope-default-regexp))
    (cscope-find-command desc cmd symbol regexp)))

(defun cscope-find-global-definition (symbol)
  (interactive (list (cscope-prompt-for-symbol "Find global definition")))
  (let* ((desc (format "Finding global definition: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd `("-L" "-1" ,symbol))
	 (regexp cscope-default-regexp))
    (cscope-find-command desc cmd symbol regexp)))

(defun cscope-find-function-calling (symbol)
  (interactive (list (cscope-prompt-for-symbol "Find function calling")))
  (let* ((desc (format "Finding function calling: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd `("-L" "-3" ,symbol))
	 (regexp cscope-default-regexp))
    (cscope-find-command desc cmd symbol regexp)))

(defun cscope-find-text-string (symbol)
  (interactive (list (cscope-prompt-for-symbol "Find text string")))
  (let* ((desc (format "Finding text string: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd `("-L" "-4" ,symbol))
	 (regexp cscope-default-regexp))
    (cscope-find-command desc cmd symbol regexp)))

(defun cscope-find-symbol-assignment (symbol)
  (interactive (list (cscope-prompt-for-symbol "Find symbol assignment")))
  (let* ((desc (format "Finding symbol assignment: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd `("-L" "-9" ,symbol))
	 (regexp cscope-default-regexp))
    (cscope-find-command desc cmd symbol regexp)))

(defun cscope-struct-regexp-func (line pattern)
  (when (string-match cscope-default-regexp line)
    (let ((file (substring line (match-beginning 1) (match-end 1)))
	  (func (substring line (match-beginning 2) (match-end 2)))
	  (line-nbr (substring line (match-beginning 3) (match-end 3)))
	  (line-str (substring line (match-beginning 4) (match-end 4))))
      (cond ((string-match-p (format "struct %s {" pattern) line)
	     (cons file `((:func ,func :line-nbr ,line-nbr :line-str ,line-str))))
	    ((string-match (format "typedef struct \\(.*\\) %s" pattern) line)
	     (cscope-find-struct-definition (match-string 1 line))
	     (cons file `((:func ,func :line-nbr ,line-nbr :line-str ,line-str))))))))

(defun cscope-find-struct-definition (symbol)
  (interactive (list (cscope-prompt-for-symbol "Find struct definition")))
  (let* ((desc (format "Finding struct definition: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd `("-L" "-1" ,symbol))
	 (regexp 'cscope-struct-regexp-func))
    (cscope-find-command desc cmd symbol regexp)))

;; database management
(defun cscope-database-default-option ()
  (append (cscope-build-default-option) '("-b")))

(defun cscope-build-find-cmd ()
  (concat "find . -name \"*.[chxsS]\" > " cscope-index-file))

(defun cscope-database-remove-files (dir)
  (mapc (lambda (file)
	  (let ((default-directory dir))
	    (if (file-exists-p file)
		(delete-file file))))
	(append cscope-database-fast-symbol-files
		(list (eval cscope-database-file)))))

(defun cscope-database-finish (output error data)
  (add-to-list 'cscope-database-list (cscope-data-dir data) t)
  (cscope-insert (format "Database created in %.2f seconds\n\n"
			 (- (cscope-get-time-seconds)
			    (cscope-data-start data)))))

(defun cscope-create-database (dir)
  (cscope-check-env)
  (let* ((default-directory dir)
	 (find-cmd (cscope-build-find-cmd))
	 (cmd (cscope-database-default-option))
	 (desc (concat "Creating "
		       (if cscope-fast-symbol
			   (propertize "Fast Symbol " 'face 'bold))
		       "cscope database ...\n"))
	 (data (make-cscope-data :dir dir
				 :desc desc))
	 (request (make-cscope-request :dir dir :cmd cmd
				       :start 'cscope-insert-initial-header
				       :fail 'cscope-insert-request-fail
				       :finish 'cscope-database-finish
				       :data data)))
    (unless (file-exists-p cscope-index-file)
      (shell-command find-cmd))
    (cscope-process-request request)))

(defun cscope-check-database ()
  (unless cscope-database-list
    (if-let ((git-repo (magit-toplevel)))
	(add-to-list 'cscope-database-list git-repo t)
      (call-interactively 'cscope-add-database)))
  (mapc (lambda (dir)
	  (let ((path (concat dir cscope-database-file))
		(fmt "Database (%s) doesn't exist, create it ? "))
	    (unless (file-exists-p path)
	      (if (yes-or-no-p (format fmt path))
		  (cscope-create-database dir)
		(cscope-abort)))))
	cscope-database-list))

(defun cscope-add-database (dir)
  (interactive "DAdd database: ")
  (if (file-exists-p (concat dir cscope-database-file))
      (add-to-list 'cscope-database-list dir t)
    (cscope-create-database dir)))

(defun cscope-reset-database ()
  (interactive)
  (setq cscope-database-list nil))

;; tree management
(defvar cscope-tree-depth-max 3)
(defvar cscope-tree-patterns nil)
(defvar cscope-tree-data-test nil)
(defvar cscope-tree-data-current nil)
(defvar cscope-tree-depth 0)

(defun cscope-tree-insert-data ()
  (mapc (lambda (elem)
	  (cscope-insert (car elem))
	  (cscope-insert "\n")
	  (mapc (lambda (te)
		  (cscope-insert (format "<-- %s\n" te)))
		(cdr elem))
	  (cscope-insert "\n"))
      cscope-tree-data-test))

(defun cscope-tree-next-finish (output error data)
  (let* ((regexp (cscope-data-regexp data))
	 (results (cscope-build-assoc-results output regexp)))
    (cscope-tree-handle-results results data)))

(defun cscope-tree-next-search (data)
  ;; add current data
  (setq cscope-tree-data-test (append cscope-tree-data-test
				      cscope-tree-data-current))
  ;; increase depth search
  (setq cscope-tree-depth (+ cscope-tree-depth 1))

  (let (patterns)

    ;; join all patterns to search
    (mapc (lambda (funcs)
	    (setq patterns (append patterns (cdr funcs))))
	  cscope-tree-data-current)

    ;; reset current data
    (setq cscope-tree-data-current nil)

    ;; set new patterns
    (setq cscope-tree-patterns patterns)

    ;; create new requests
    (mapc (lambda (pattern)
	    (let* ((dir (cscope-data-dir data))
		   (cmd (append (cscope-find-default-option) `("-L" "-3" ,pattern)))
		   (request (make-cscope-request :dir dir :cmd cmd
						 :finish 'cscope-tree-next-finish
						 :data data)))
	      (cscope-process-request request)))
	  patterns)))

(defun cscope-tree-handle-results (results data)
  (let ((pattern (pop cscope-tree-patterns))
	funcs)
    ;; collect functions who called this pattern
    (when results
      (mapc (lambda (result)
	      (mapc (lambda (elem)
		      (if-let ((func (plist-get elem :func)))
			(add-to-list 'funcs func t)))
		    (cdr result)))
	    results)
      ;; add to current data at this tree depth
      (add-to-list 'cscope-tree-data-current (cons pattern funcs) t))

    ;; if we still have patterns, that means other data are coming
    (unless cscope-tree-patterns
      (if (< cscope-tree-depth cscope-tree-depth-max)
	  (cscope-tree-next-search data)
	(cscope-tree-insert-data)
	(cscope-insert (format "Search time = %.2f seconds\n\n"
			       (- (cscope-get-time-seconds)
				  (cscope-data-start data))))))))

(defun cscope-tree-finish (output error data)
  (let* ((regexp (cscope-data-regexp data))
	 (results (cscope-build-assoc-results output regexp)))
    (if results
	(cscope-tree-handle-results results data)
      (cscope-insert " --- No matches were found ---\n\n")
      (cscope-insert (format "Search time = %.2f seconds\n\n"
			     (- (cscope-get-time-seconds)
				(cscope-data-start data)))))))

(defun cscope-tree-function-calling (symbol)
  (interactive (list (cscope-prompt-for-symbol "Tree function calling")))

  (cscope-check-env)
  (cscope-check-database)

  (setq cscope-tree-patterns nil)
  (setq cscope-tree-data-test nil)
  (setq cscope-tree-data-current nil)
  (setq cscope-tree-depth 0)

  (let* ((dir (car cscope-database-list))
	 (desc (format "Tree function calling: %s\n"
		       (propertize symbol 'face 'bold)))
	 (cmd (append (cscope-find-default-option) `("-L" "-3" ,symbol)))
	 (regexp cscope-default-regexp)
	 (data (make-cscope-data :dir dir
				 :desc desc
				 :regexp regexp))
	 (request (make-cscope-request :dir dir
				       :cmd cmd
				       :start 'cscope-insert-initial-header
				       :finish 'cscope-tree-finish
				       :data data)))
    (add-to-list 'cscope-tree-patterns symbol t)
    (cscope-process-request request)))

(provide 'cscope)
