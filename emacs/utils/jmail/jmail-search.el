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

(require 'jmail-process)
(require 'jmail-view)

(define-derived-mode jmail-search-mode fundamental-mode
  "jmail search"
  (jmail-search--insert-header-line)
  (setq-local hl-line-face 'notmuch-header-highlight-face)
  (toggle-read-only t))

(define-key jmail-search-mode-map [return] 'jmail-search-enter)
(define-key jmail-search-mode-map "g" 'jmail-search-refresh)
(define-key jmail-search-mode-map "m" 'jmail-search-mark-as-read)
(define-key jmail-search-mode-map "M" 'jmail-search-mark-all-as-read)
(define-key jmail-search-mode-map "n" 'jmail-search-next)
(define-key jmail-search-mode-map "p" 'jmail-search-previous)
(define-key jmail-search-mode-map "q" 'jmail-search-quit)
(define-key jmail-search-mode-map "r" 'jmail-search-toggle-related)
(define-key jmail-search-mode-map "t" 'jmail-search-toggle-thread)

(defvar jmail-search--format "%s %-11s %-16s  %s")

;;; Internal Variables

(defconst jmail-search--buffer-name "*jmail-search*")

(defvar jmail-search--current nil)

(defvar jmail-search--overlays nil)

(defvar jmail-search--need-index nil)

;;; Internal Functions

(defmacro with-jmail-search-buffer (&rest body)
  `(let ((inhibit-read-only t))
     (if (get-buffer jmail-search--buffer-name)
	 (with-current-buffer jmail-search--buffer-name
	          ,@body)
       (with-current-buffer (get-buffer-create jmail-search--buffer-name)
	 (jmail-search-mode)
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
  (if-let* ((from (car (plist-get object :from)))
	    (name (car from)))
      (propertize (truncate-string-to-width name 16)
		  'face 'font-lock-variable-name-face)
    (propertize (truncate-string-to-width (cdr from) 16)
		'face 'font-lock-variable-name-face)))

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

(defun jmail-search--delete-overlay (pos)
  (when-let ((overlay (jmail-search--find-overlay pos)))
    (delete-overlay overlay)
    (when jmail-search--overlays
      (delete overlay jmail-search--overlays))))

(defun jmail-search--delete-all-overlays ()
  (with-jmail-search-buffer
   (save-excursion
     (goto-char (point-min))
     (while (not (eobp))
       (jmail-search--delete-overlay (line-beginning-position))
       (next-line)))
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
       (rename-file path new-path)
       (setq jmail-search--need-index t)))))
  
(defun jmail-search--args (query thread related)
  (let ((option "find")
	(default-args (list "--reverse" "--format=sexp" query)))
    (when thread
      (push "--threads" default-args))
    (when related
      (push "--include-related" default-args))
    (push option default-args)))

;;; External Functions

(defun jmail-search-refresh ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (plist-get jmail-search--current :related)))
      (jmail-search query thread related))))

(defun jmail-search-toggle-related ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (not (plist-get jmail-search--current :related))))
    (jmail-search query thread related))))

(defun jmail-search-toggle-thread ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (not (plist-get jmail-search--current :thread)))
	  (related (plist-get jmail-search--current :related)))
    (jmail-search query thread related))))

(defun jmail-search-quit ()
  (interactive)
  (jmail-process-kill-all)
  (when jmail-search--need-index
    (jmail-update-index))
  (jmail-view-quit)
  (jmail-search--delete-all-overlays)
  (with-jmail-search-buffer
   (kill-buffer))
  (jmail-menu))

(defun jmail-search-enter ()
  (interactive)
  (when-let ((object (text-properties-at (point))))
    (jmail-view object jmail-search--buffer-name)
    (jmail-search--mark-as-read (line-beginning-position))))

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

(defun jmail-search (query thread related)
  (setq jmail-search--need-index nil)
  (with-jmail-search-buffer
   (jmail-search--delete-all-overlays)
   (erase-buffer)
   (setq-local truncate-lines t)
   (setq-local jmail-search--current `(:query ,query
				       :thread ,thread
				       :related ,related))
   (switch-to-buffer (current-buffer)))
  (jmail-process (make-jprocess :dir default-directory
				:program "mu"
				:args (jmail-search--args query thread related)
				:cb 'jmail-search--process-results)))

(provide 'jmail-search)
