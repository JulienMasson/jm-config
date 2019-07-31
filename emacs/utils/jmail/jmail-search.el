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
    (define-key map "D" 'jmail-search-delete-message)
    (define-key map "M" 'jmail-search-mark-all-as-read)
    (define-key map "T" 'jmail-search-only-this-thread)
    (define-key map "g" 'jmail-search-refresh)
    (define-key map "m" 'jmail-search-mark-as-read)
    (define-key map "n" 'jmail-search-next)
    (define-key map "p" 'jmail-search-previous)
    (define-key map "q" 'jmail-search-quit)
    (define-key map "r" 'jmail-search-toggle-related)
    (define-key map "t" 'jmail-search-toggle-thread)
    (define-key map (kbd "M-<left>") 'jmail-search-previous-query)
    (define-key map (kbd "M-<right>") 'jmail-search-next-query)
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

;;; Internal Variables

(defconst jmail-search--process-buffer-name "*jmail-search-process*")

(defvar jmail-search--process-next nil)

(defconst jmail-search--buffer-name "*jmail-search*")

(defconst jmail-search--format "%s %-11s %-16s  %s")

(defvar-local jmail-search--current nil)

(defvar-local jmail-search--overlays nil)

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
   (cl-multiple-value-bind (date from subject)
       (jmail-search--parse-object object)
     (save-excursion
       (goto-char (point-max))
       (insert (format jmail-search--format " " date from subject))
       (add-text-properties (line-beginning-position) (line-end-position) object)
       (jmail-search--insert-flag (line-beginning-position) object)
       (insert "\n")))))

(defun jmail-search--remove-flag (pos)
  (when-let ((overlay (jmail-search--find-overlay pos)))
    (overlay-put overlay 'before-string "  ")))

(defun jmail-search--set-property (prop value)
  (put-text-property (line-beginning-position)
		     (line-end-position)
		     prop value))

(defun jmail-search--mark-as-read (point)
  (with-jmail-search-buffer
   (let* ((object (text-properties-at point))
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

(defun jmail-search--args (query thread related)
  (let ((option "find")
	(default-args (list "--reverse" "--format=sexp" query)))
    (when thread
      (push "--threads" default-args))
    (when related
      (push "--include-related" default-args))
    (push option default-args)))

(defun jmail-search--process-objects (buffer)
  (let (object)
    (while (setq object (jmail-extract-sexp-object buffer))
      (jmail-search--process-results object))))

(defun jmail-search--process-sentinel (process status)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (jmail-search--process-objects (process-buffer process))
    (kill-buffer (process-buffer process)))
  (when jmail-search--process-next
    (jmail-search--process-run jmail-search--process-next)
    (setq jmail-search--process-next nil)))

(defun jmail-search--process-filter (process str)
  (unless (eq (process-status process) 'signal)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert str)
      (jmail-search--process-objects (current-buffer)))))

(defun jmail-search--process-run (args)
  (when-let* ((program (jmail-find-program-from-top jmail-index-program))
	      (buffer (get-buffer-create jmail-search--process-buffer-name))
	      (process (apply 'start-file-process "jmail-search-process" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-search--process-filter)
    (set-process-sentinel process 'jmail-search--process-sentinel)))

(defun jmail-search--process-kill-if-running ()
  (when-let* ((process (get-buffer-process jmail-search--process-buffer-name))
	      (status (process-status process)))
    (when (eq status 'run)
      (interrupt-process process))))

(defun jmail-search--process-kill-all ()
  (setq jmail-search--process-next nil)
  (jmail-search--process-kill-if-running))

(defun jmail-search--process (args)
  (if (jmail-search--process-kill-if-running)
      (setq jmail-search--process-next args)
    (jmail-search--process-run args)))

(defun jmail-search--run (query thread related save)
  (unless (get-buffer jmail-search--buffer-name)
    (with-current-buffer (get-buffer-create jmail-search--buffer-name)
      (jmail-search-mode)))
  (with-jmail-search-buffer
   (jmail-search--delete-all-overlays)
   (erase-buffer)
   (setq jmail-search--current `(:query ,query
				 :thread ,thread
				 :related ,related))
   (when save
     (push jmail-search--current jmail-search--saved))
   (switch-to-buffer (current-buffer)))
  (jmail-search--process (jmail-search--args query thread related)))

;;; External Functions

(defun jmail-search-refresh ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related nil))))

(defun jmail-search-toggle-related ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (not (plist-get jmail-search--current :related))))
    (jmail-search--run query thread related nil))))

(defun jmail-search-toggle-thread ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (not (plist-get jmail-search--current :thread)))
	  (related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread related nil))))

(defun jmail-search-quit ()
  (interactive)
  (jmail-search--process-kill-all)
  (jmail-view-quit)
  (jmail-search--delete-all-overlays)
  (with-jmail-search-buffer
   (kill-buffer))
  (jmail))

(defun jmail-search-enter ()
  (interactive)
  (with-jmail-search-buffer
   (when-let ((object (text-properties-at (point))))
     (jmail-search--mark-as-read (line-beginning-position))
     (jmail-view (plist-get object :path) (current-buffer)))))

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
