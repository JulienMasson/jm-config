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
    (define-key map "D" 'cscope-erase-all)
    (define-key map "n" 'cscope-next-file)
    (define-key map "p" 'cscope-previous-file)
    (define-key map "q" 'cscope-quit)
    (define-key map (kbd "C-n") 'cscope-next-request)
    (define-key map (kbd "C-p") 'cscope-previous-request)
    map))

(define-derived-mode cscope-mode fundamental-mode
  "cscope"
  (toggle-read-only t))

;; External vars
(defvar cscope-database-list nil)

;; Internal vars
(defvar cscope-buffer-name "*cscope*")
(defvar cscope-result-separator (concat (make-string 80 (string-to-char "="))))
(defvar cscope-file-entry "***")
(defvar cscope-index-file "cscope.files")
(defvar cscope-database-file "cscope.out")
(defvar cscope-default-regexp
  "^\\(.*\\)[ \t]+\\(.*\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)")
(defvar cscope-prompt-history nil)

;; utils
(defun cscope-abort ()
  (signal 'quit t))

(defun cscope-get-time-seconds ()
  (string-to-number (format-time-string "%s.%3N" (current-time))))

(defun cscope-build-default-option ()
  (list "-k" "-i" (eval cscope-index-file)
	"-f" (eval cscope-database-file)))

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
      (cscope-mode))))

(defun cscope-insert-request-fail (output error data)
  (cscope-insert (concat (propertize "ERROR: " 'face 'error)
			 error "\n"
			 (mapconcat 'identity output "\n") "\n")))

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

;; mode functions
(defun cscope-enter ()
  (interactive)
  (let ((file (get-text-property (point) 'cscope-file))
	(line (get-text-property (point) 'cscope-line)))
    (when file
      (cscope-switch-to-buffer (find-file-noselect file))
      (if line (goto-line line)))))

(defun cscope-quit ()
  (interactive)
  (kill-buffer cscope-buffer-name))

(defun cscope-next-file ()
  (interactive)
  (with-current-buffer cscope-buffer-name
    (end-of-line)
    (search-forward cscope-file-entry nil t)
    (beginning-of-line)))

(defun cscope-previous-file ()
  (interactive)
  (with-current-buffer cscope-buffer-name
    (beginning-of-line)
    (search-backward cscope-file-entry nil t)
    (beginning-of-line)))

(defun cscope-next-request ()
  (interactive)
  (with-current-buffer cscope-buffer-name
    (end-of-line)
    (search-forward cscope-result-separator nil t)
    (beginning-of-line)))

(defun cscope-previous-request ()
  (interactive)
  (with-current-buffer cscope-buffer-name
    (beginning-of-line)
    (search-backward cscope-result-separator nil t)
    (beginning-of-line)))

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

;; find management
(defun cscope-jump-first-result ()
  (with-current-buffer cscope-buffer-name
    (goto-char (point-max))
    (search-backward cscope-result-separator nil t)
    (search-forward cscope-file-entry nil t)
    (next-line)
    (cscope-enter)))

(defun cscope-parse-line (line pattern regexp)
  (if (functionp regexp)
      (funcall regexp line pattern)
    (when (string-match regexp line)
      (let ((file (substring line (match-beginning 1) (match-end 1)))
	    (func (substring line (match-beginning 2) (match-end 2)))
	    (line-nbr (substring line (match-beginning 3) (match-end 3)))
	    (line-str (substring line (match-beginning 4) (match-end 4))))
	(cons file `((:func ,func :line-nbr ,line-nbr :line-str ,line-str)))))))

(defun cscope-build-assoc-results (output pattern regexp)
  (let (results)
    (mapc (lambda (line)
    	    (when-let ((result (cscope-parse-line line pattern regexp)))
	      (if-let ((data (assoc-default (car result) results)))
	      	  (setcdr (assoc (car result) results)
	      		  (add-to-list 'data (cadr result) t))
    		(add-to-list 'results result t))))
	  output)
    results))

(defun cscope-propertize-line (beg end file line)
  (with-current-buffer cscope-buffer-name
    (let ((inhibit-read-only t)
	  plist)
      (setq plist (plist-put plist 'cscope-file file))
      (when line
	(setq plist (plist-put plist 'cscope-line line)))
      (add-text-properties beg (point-max) plist))))

(defun cscope-insert-results (results)
  (mapc (lambda (result)
	  (let* ((file (car result))
		 (data (cdr result))
		 (beg (cscope-point-max))
		 (dir (cscope-request-dir cscope-current-request)))
	    ;; insert file
	    (cscope-insert (propertize (format "%s %s:" cscope-file-entry file)
				       'face 'font-lock-constant-face))
	    (cscope-propertize-line beg (cscope-point-max) (concat dir file) nil)
	    (cscope-insert "\n")

	    ;; insert data
	    (mapc (lambda (elem)
	    	    (let ((func (plist-get elem :func))
	    		  (line-nbr (plist-get elem :line-nbr))
	    		  (line-str (plist-get elem :line-str))
	    		  (beg (cscope-point-max))
	    		  plist)
	    	      (cscope-insert
	    	       (format "%-35s %s"
	    		       (format "%s[%s]"
	    			       (propertize func 'face 'font-lock-type-face)
	    			       (propertize line-nbr 'face 'font-lock-string-face))
	    		       line-str))
	    	      (cscope-propertize-line beg (cscope-point-max)
	    				      (concat dir file)
	    				      (string-to-number line-nbr))
	    	      (cscope-insert "\n")))
	    	  data)
	    (cscope-insert "\n")))
	results))

(defun cscope-find-finish (output error data)
  (let* ((regexp (cscope-data-regexp data))
	 (pattern (cscope-data-pattern data))
	 (results (cscope-build-assoc-results output pattern regexp)))
    (if results
	(cscope-insert-results results)
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
  (let* ((cmd (append (cscope-build-default-option) cmd))
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
  (let ((symbol (symbol-name (symbol-at-point))))
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
(defun cscope-build-find-cmd ()
  (concat "find . -name \"*.[chxsS]\" > " cscope-index-file))

(defun cscope-database-finish (output error data)
  (add-to-list 'cscope-database-list (cscope-data-dir data) t)
  (cscope-insert (format "Database created in %.2f seconds\n\n"
			 (- (cscope-get-time-seconds)
			    (cscope-data-start data)))))

(defun cscope-create-database (dir)
  (cscope-check-env)
  (let* ((default-directory dir)
	 (find-cmd (cscope-build-find-cmd))
	 (cmd (append (cscope-build-default-option) '("-b")))
	 (desc "Creating database cscope ...\n")
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

(provide 'cscope)
