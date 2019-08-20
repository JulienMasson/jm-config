;;; jmail-search.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-07-12

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'jmail-view)

;;; Mode

(defvar jmail-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" 'jmail-search-more-history-limit)
    (define-key map "A" 'jmail-search-apply-patch-series)
    (define-key map "D" 'jmail-search-delete-message)
    (define-key map "M" 'jmail-search-mark-all-as-read)
    (define-key map "T" 'jmail-search-only-this-thread)
    (define-key map "a" 'jmail-search-apply-patch)
    (define-key map "f" 'jmail-search-toggle-folding-thread-all)
    (define-key map "g" 'jmail-search-refresh)
    (define-key map "m" 'jmail-search-mark-as-read)
    (define-key map "n" 'jmail-search-next)
    (define-key map "p" 'jmail-search-previous)
    (define-key map "q" 'jmail-search-quit)
    (define-key map "r" 'jmail-search-toggle-related)
    (define-key map "t" 'jmail-search-toggle-thread)
    (define-key map (kbd "C-<down>") 'jmail-search-next-thread)
    (define-key map (kbd "C-<up>") 'jmail-search-previous-thread)
    (define-key map (kbd "M-<left>") 'jmail-search-previous-query)
    (define-key map (kbd "M-<right>") 'jmail-search-next-query)
    (define-key map (kbd "TAB") 'jmail-search-toggle-folding-thread)
    (define-key map [return] 'jmail-search-enter)
    map)
  "Keymap for `jmail-search-mode'")

(define-derived-mode jmail-search-mode fundamental-mode
  "jmail search"
  (jmail-search--insert-header-line)
  (setq-local hl-line-face 'jmail-search-hl-line)
  (setq truncate-lines t)
  (toggle-read-only t))

;;; Faces

(defface jmail-search-hl-line
  '((t :inherit region :weight bold :underline t))
  "Face with which to highlight the current line in `jmail-search-mode'"
  :group 'jmail)

(defface jmail-search-overlay-fold-face
  '((t :inherit 'font-lock-keyword-face))
  "Default face used to display `jmail-search--overlay-string'"
  :group 'jmail)

(defface jmail-search-results-footer-face
  '((t :slant italic))
  "Default face used to display results footer"
  :group 'jmail)

;;; Customization

(defcustom jmail-search-limit nil
  "Limit to a number of search results, if nil no limit"
  :type 'integer
  :group 'jmail)

;;; Internal Variables

(defconst jmail-search--process-buffer-name "*jmail-search-process*")

(defvar jmail-search--process-next nil)

(defconst jmail-search--buffer-name "*jmail-search*")

(defconst jmail-search--format "%s %-11s %-16s  %s")

(defvar-local jmail-search--current nil)

(defvar-local jmail-search--current-limit nil)

(defvar-local jmail-search--skip-objects nil)

(defvar-local jmail-search--count-objects nil)

(defvar-local jmail-search--overlays nil)

(defvar-local jmail-search--fold-overlays nil)

(defconst jmail-search--overlay-string " [...]")

(defvar jmail-search--saved nil)

(defvar jmail-search--saved-index 0)

;;; Internal Functions

(defmacro with-jmail-search-buffer (&rest body)
  `(when (get-buffer jmail-search--buffer-name)
     (with-current-buffer jmail-search--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-search--insert-header-line ()
  (with-jmail-search-buffer
   (setq header-line-format (format jmail-search--format " 🔽" "Date" "From" "Subject"))
   (force-mode-line-update)))

(defun jmail-search--subject-str (object)
  (let* ((subject (plist-get object :subject))
	 (thread (plist-get object :thread)))
    (if thread
	(let* ((level (plist-get thread :level))
	       (last-child (plist-get thread :last-child))
	       (spaces (make-string (* level 4) (string-to-char " "))))
	  (cond ((= level 0)
		 subject)
		((= level 1)
		 (if last-child
		     (format "  ┗━▶ %s" subject)
		   (format "  ┣━▶ %s" subject)))
		((> level 1)
		 (if last-child
		     (format "  ┃%s┗━▶ %s" spaces subject)
		   (format "  ┃%s┣━▶ %s" spaces subject)))))
      subject)))

(defun jmail-search--date-str (object)
  (propertize (format-time-string "%F" (plist-get object :date))
	      'face 'font-lock-comment-face))

(defun jmail-search--from-str (object)
  (if-let ((from (car (plist-get object :from))))
      (if-let ((name (car from)))
	  (propertize (truncate-string-to-width name 16)
		      'face 'font-lock-variable-name-face)
	(propertize (truncate-string-to-width (cdr from) 16)
		    'face 'font-lock-variable-name-face))
    (propertize "unknown" 'face 'font-lock-variable-name-face)))

(defun jmail-search--parse-object (object)
  (list (jmail-search--date-str object)
	(jmail-search--from-str object)
	(jmail-search--subject-str object)))

(defun jmail-search--insert-overlay (start end str)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string str)
    (add-to-list 'jmail-search--overlays overlay)))

(defun jmail-search--find-overlay (pos)
  (cl-find-if (lambda (ov)
	      (and (<= (overlay-start ov) pos)
		   (>= (overlay-end ov) pos)))
	    jmail-search--overlays))

(defun jmail-search--delete-all-overlays ()
  (with-jmail-search-buffer
   (delete-all-overlays)
   (setq jmail-search--fold-overlays nil)
   (setq jmail-search--overlays nil)))

(defun jmail-search--insert-flag (start object)
  (let ((flags (plist-get object :flags))
	(unread (propertize "U " 'face 'error))
	(flagged (propertize "S " 'face 'font-lock-warning-face)))
    (cond ((member 'unread flags)
	   (jmail-search--insert-overlay start (+ start 2) unread))
	  ((member 'flagged flags)
	   (jmail-search--insert-overlay start (+ start 2) flagged))
	  (t (jmail-search--insert-overlay start (+ start 2) "  ")))))

(defun jmail-search--process-results (object)
  (with-jmail-search-buffer
   (unless (and jmail-search--skip-objects
		(> jmail-search--skip-objects jmail-search--count-objects))
     (cl-multiple-value-bind (date from subject)
	 (jmail-search--parse-object object)
       (save-excursion
	 (goto-char (point-max))
	 (insert (format jmail-search--format " " date from subject))
	 (add-text-properties (line-beginning-position) (line-end-position) object)
	 (jmail-search--insert-flag (line-beginning-position) object)
	 (insert "\n"))))
   (setq jmail-search--count-objects (+ jmail-search--count-objects 1))))

(defun jmail-search--remove-flag (pos)
  (when-let ((overlay (jmail-search--find-overlay pos)))
    (overlay-put overlay 'before-string "  ")))

(defun jmail-search--set-property (prop value)
  (put-text-property (line-beginning-position)
		     (line-end-position)
		     prop value))

(defun jmail-search--mark-as-read (point)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (new-path path))
     (jmail-search--remove-flag point)
     (when (member 'new flags)
       (setq flags (remove 'new flags))
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string "new" "cur" new-path)))
     (when (member 'unread flags)
       (setq flags (remove 'unread flags))
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string ",$" ",S" new-path)))
     (unless (string= path new-path)
       (jmail-search--set-property :path new-path)
       (rename-file path new-path)))))

(defun jmail-search--args (query thread related limit)
  (let ((option "find")
	(default-args (list "--reverse" "--format=sexp" query)))
    (when limit
      (push (format "--maxnum=%d" (+ limit 1)) default-args))
    (when thread
      (push "--threads" default-args))
    (when related
      (push "--include-related" default-args))
    (push option default-args)))

(defun jmail-search--insert-more-history-button ()
  (with-jmail-search-buffer
   (save-excursion
     (goto-char (point-max))
     (insert-text-button
      (substitute-command-keys
       (format "Type \\<%s>\\[%s] to show more history"
               'jmail-search-mode-map
               'jmail-search-more-history-limit))
      'action (lambda (_button)
		(jmail-search-more-history-limit))))))

(defun jmail-search--delete-more-history-button ()
  (with-jmail-search-buffer
   (save-excursion
     (goto-char (- (point-max) 1))
     (when (button-at (point))
       (delete-region (line-beginning-position) (point-max))))))

(defun jmail-search--insert-end-of-results ()
  (insert (propertize (if (eq (point-min) (point-max))
			  "No results found !!!!"
			"End of search results.")
		      'face 'jmail-search-results-footer-face)))

(defun jmail-search--delete-last-result ()
  (with-jmail-search-buffer
   (save-excursion
     (goto-char (- (point-max) 1))
     (mapc (lambda (overlay)
     	     (setq jmail-search--overlays
		   (delete overlay jmail-search--overlays))
     	     (delete-overlay overlay))
     	   (overlays-at (line-beginning-position)))
     (delete-region (line-beginning-position) (point-max)))))

(defun jmail-search--insert-footer ()
  (with-jmail-search-buffer
   (save-excursion
     (goto-char (point-max))
     (if (or (not jmail-search--current-limit)
	     (<= (count-lines (point-min) (point-max))
		 jmail-search--current-limit))
	 (jmail-search--insert-end-of-results)
       (jmail-search--delete-last-result)
       (jmail-search--insert-more-history-button)))))

(defun jmail-search--process-objects (buffer)
  (let (object)
    (while (setq object (jmail-extract-sexp-object buffer))
      (jmail-search--process-results object))))

(defun jmail-search--process-sentinel (process status)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (jmail-search--process-objects (process-buffer process)))
  (jmail-search--insert-footer)
  (if jmail-search--process-next
      (progn
	(jmail-search--setup-env)
	(jmail-search--process-run jmail-search--process-next)
	(setq jmail-search--process-next nil))
    (kill-buffer (process-buffer process))))

(defun jmail-search--process-filter (process str)
  (unless (eq (process-status process) 'signal)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert str)
      (jmail-search--process-objects (current-buffer)))))

(defun jmail-search--process-run (args)
  (when-let* ((program (jmail-find-program jmail-index-program))
	      (buffer (get-buffer-create jmail-search--process-buffer-name))
	      (process (apply 'start-file-process "jmail-search-process" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-search--process-filter)
    (set-process-sentinel process 'jmail-search--process-sentinel)))

(defun jmail-search--stop-process ()
  (setq jmail-search--process-next nil)
  (jmail-terminate-process-buffer jmail-search--process-buffer-name))

(defun jmail-search--process (args)
  (if (jmail-terminate-process-buffer jmail-search--process-buffer-name)
      (setq jmail-search--process-next args)
    (jmail-search--process-run args)))

(defun jmail-search--setup-env (&optional skip)
  (with-jmail-search-buffer
   (setq jmail-search--count-objects 0)
   (unless skip
     (jmail-search--delete-all-overlays)
     (erase-buffer)
     (setq jmail-search--skip-objects nil)
     (setq jmail-search--current-limit jmail-search-limit))))

(defun jmail-search--run (query thread related &optional save skip)
  (unless (get-buffer jmail-search--buffer-name)
    (with-current-buffer (get-buffer-create jmail-search--buffer-name)
      (jmail-search-mode)))
  (with-jmail-search-buffer
   (jmail-search--setup-env skip)
   (setq jmail-search--current `(:query ,query
				 :thread ,thread
				 :related ,related))
   (when save
     (push jmail-search--current jmail-search--saved))
   (switch-to-buffer (current-buffer))
   (jmail-search--process (jmail-search--args query thread related
					      jmail-search--current-limit))))

(defun jmail-search--thread-level-at-point ()
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (thread (plist-get object :thread)))
       (plist-get thread :level))))

(defun jmail-search--goto-root-thread ()
  (when-let* ((object (text-properties-at (point)))
	      (thread (plist-get object :thread))
	      (level (plist-get thread :level)))
    (unless (zerop level)
      (previous-line)
      (jmail-search--goto-root-thread))))

(defmacro jmail-search--foreach-line-thread (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (jmail-search--goto-root-thread)
      (next-line)
      (while (and (jmail-search--thread-level-at-point)
		  (not (zerop (jmail-search--thread-level-at-point))))
	,@body
	(next-line)))))

(defun jmail-search--thread-range ()
  (if-let ((level (jmail-search--thread-level-at-point)))
      (let ((start (line-end-position))
	    end)
	(save-excursion
	  (forward-line)
	  (while (and (jmail-search--thread-level-at-point)
		      (< level (jmail-search--thread-level-at-point))
		      (not (eobp)))
	    (forward-line))
	  (setq end (- (point) 1)))
	(list start end))
    (list (point) (point))))

(defun jmail-search--find-fold-overlay (start end)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) start)
		     (>= (overlay-end ov) end)))
	      jmail-search--fold-overlays))

(defun jmail-search--remove-fold-overlay (overlay)
  (setq jmail-search--fold-overlays (remove overlay jmail-search--fold-overlays))
  (delete-overlay overlay))

(defun jmail-search--add-fold-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (add-to-list 'jmail-search--fold-overlays overlay)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string
		 (propertize jmail-search--overlay-string
			     'face 'jmail-search-overlay-fold-face))))

;;; External Functions

(defun jmail-search-more-history-limit ()
  (interactive)
  (with-jmail-search-buffer
   (when (and jmail-search--current jmail-search-limit
	      (button-at (- (point-max) 1)))
     (jmail-search--delete-more-history-button)
     (setq jmail-search--skip-objects jmail-search--current-limit)
     (setq jmail-search--current-limit
	   (+ jmail-search-limit jmail-search--current-limit))
     (let ((query (plist-get jmail-search--current :query))
	   (thread (plist-get jmail-search--current :thread))
	   (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related nil t)))))

(defun jmail-search-apply-patch-series (dir)
  (interactive "DApply patch series: ")
  (jmail-search--foreach-line-thread
      (when-let* ((object (text-properties-at (point)))
		  (thread (plist-get object :thread))
		  (level (plist-get thread :level))
		  (msg (plist-get object :path))
		  (subject (plist-get object :subject))
		  (default-directory dir))
	(when (and (string-match "^\\[PATCH " subject)
		   (= level 1))
	  (shell-command (concat "git am " msg))))))

(defun jmail-search-apply-patch (dir)
  (interactive "DApply patch: ")
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (msg (plist-get object :path))
	       (subject (plist-get object :subject))
	       (default-directory dir))
     (when (string-match "^\\[PATCH " subject)
       (shell-command (concat "git am " msg))))))

(defun jmail-search-previous-thread ()
  (interactive)
  (previous-line)
  (jmail-search--goto-root-thread))

(defun jmail-search-next-thread ()
  (interactive)
  (next-line)
  (when-let ((level (jmail-search--thread-level-at-point)))
    (while (and level (not (zerop level)))
      (next-line)
      (setq level (jmail-search--thread-level-at-point)))))

(defun jmail-search-toggle-folding-thread ()
  (interactive)
  (cl-multiple-value-bind (start end)
      (jmail-search--thread-range)
    (unless (= start end)
      (if-let ((overlay (jmail-search--find-fold-overlay start end)))
	  (jmail-search--remove-fold-overlay overlay)
	(jmail-search--add-fold-overlay start end)))))

(defun jmail-search-toggle-folding-thread-all ()
  (interactive)
  (if jmail-search--fold-overlays
      (mapc #'jmail-search--remove-fold-overlay
	    jmail-search--fold-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(jmail-search-toggle-folding-thread)
	(line-move 1)))))

(defun jmail-search-refresh ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related))))

(defun jmail-search-toggle-related ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (not (plist-get jmail-search--current :related))))
    (jmail-search--run query thread related))))

(defun jmail-search-toggle-thread ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (not (plist-get jmail-search--current :thread)))
	  (related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread related))))

(defun jmail-search-quit ()
  (interactive)
  (jmail-search--stop-process)
  (jmail-view-quit)
  (jmail-search--delete-all-overlays)
  (with-jmail-search-buffer
   (kill-buffer))
  (jmail))

(defun jmail-search-enter ()
  (interactive)
  (with-jmail-search-buffer
   (if (button-at (point))
       (push-button)
     (when-let* ((object (text-properties-at (point)))
		 (path (plist-get object :path)))
       (jmail-search--mark-as-read (line-beginning-position))
       (jmail-view path (current-buffer))))))

(defun jmail-search-next ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer jmail-search--buffer-name))
    (switch-to-buffer-other-window jmail-search--buffer-name))
  (with-jmail-search-buffer
   (next-line)
   (jmail-search-enter)))

(defun jmail-search-previous ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer jmail-search--buffer-name))
    (switch-to-buffer-other-window jmail-search--buffer-name))
  (with-jmail-search-buffer
   (previous-line)
   (jmail-search-enter)))

(defun jmail-search-next-query ()
  (interactive)
  (let* ((size (length jmail-search--saved))
	 (index (if (zerop jmail-search--saved-index)
		    (- size 1)
		  (- jmail-search--saved-index 1)))
	 (search (nth index jmail-search--saved)))
    (setq jmail-search--current search)
    (setq jmail-search--saved-index index)
    (jmail-search-refresh)))

(defun jmail-search-previous-query ()
  (interactive)
  (let* ((size (length jmail-search--saved))
	 (index (mod (+ jmail-search--saved-index 1) size))
	 (search (nth index jmail-search--saved)))
    (setq jmail-search--current search)
    (setq jmail-search--saved-index index)
    (jmail-search-refresh)))

(defun jmail-search-delete-message ()
  (interactive)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path)))
     (delete-file path)
     (jmail-search-refresh))))

(defun jmail-search-only-this-thread ()
  (interactive)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (message-id (plist-get object :message-id))
	       (query (concat "msgid:" message-id)))
     (setq jmail-search--saved-index 0)
     (jmail-search--run query t t t))))

(defun jmail-search-mark-as-read ()
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(deactivate-mark)
	(save-excursion
	  (goto-char beg)
	  (while (<= (point) end)
	    (jmail-search--mark-as-read (line-beginning-position))
	    (next-line))))
    (jmail-search--mark-as-read (line-beginning-position))))

(defun jmail-search-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (jmail-search-mark-as-read))

(defun jmail-search (query)
  (setq jmail-search--saved-index 0)
  (jmail-search--run query t nil t))

(provide 'jmail-search)
