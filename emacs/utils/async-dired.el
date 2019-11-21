;;; async-dired.el --- Asynchronous Dired Functions

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

(require 'dired)

;;; Internal Variables

(defconst async-dired--rsync-program "rsync")

(defconst async-dired--du-program "du")

(defvar async-dired--ongoing-actions nil)

(defvar-local async-dired--current-files nil)

(defvar-local async-dired--current-count nil)

(defvar-local async-dired--current-total nil)

;;; Internal Functions

(defmacro with-async-dired-buffer (process &rest body)
  (declare (indent 2))
  `(when-let (buffer (assoc-default ,process async-dired--ongoing-actions))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
	 ,@body
	 (force-mode-line-update)))))

(defun async-dired--remove-action (process)
  (setq async-dired--ongoing-actions
	(assoc-delete-all process async-dired--ongoing-actions)))

(defun async-dired--update-modeline (process progress)
  (with-async-dired-buffer process
    (setq mode-name (format "Async Dired: %s%%" progress))))

(defun async-dired--reset-buffer (process)
  (with-async-dired-buffer process
    (dired-sort-set-modeline)
    (revert-buffer)))

(defun async-dired--create-buffer (action)
  (let* ((name (format "*async-dired-%s*" action))
	 (buffer (generate-new-buffer-name name)))
    (get-buffer-create buffer)))

(defun async-dired--process-sentinel (process status)
  (async-dired--reset-buffer process)
  (async-dired--remove-action process)
  (let ((buffer (process-buffer process)))
    (if (zerop (process-exit-status process))
	(kill-buffer buffer)
      (switch-to-buffer-other-window buffer))))

;; copy
(defun async-dired--get-copy-progress (output)
  (when-let* ((lines (split-string output "\n"))
	      (line (last lines))
	      (regexp " \\([0-9]+\\)%"))
    (when (string-match regexp str)
      (match-string 1 str))))

(defun async-dired--process-filter-copy (process str)
  (let* ((output (replace-regexp-in-string "" "\n" str))
	 (progress (async-dired--get-copy-progress output)))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert output)
      (when progress
	(async-dired--update-modeline process progress)))))

(defun async-dired--rsync-prompt (marks)
  (let* ((nbr (length marks))
	 (msg (propertize (if (> nbr 1)
			      (format "* [ %d files ]" nbr)
			    (file-name-nondirectory (car marks)))
			 'face 'warning)))
  (read-directory-name (format "Rsync %s to: " msg))))

(defun async-dired--rsync-get-args (marks dest)
  (append (list "--archive" "--compress" "--info=progress2")
	  (mapcar (lambda (mark)
		    (replace-regexp-in-string "/ssh:" "" mark))
		  marks)
	  (list dest)))

(defun async-dired--rsync ()
  (when-let* ((marks (dired-get-marked-files))
	      (dest (async-dired--rsync-prompt marks))
	      (default-directory dest)
	      (program (executable-find async-dired--rsync-program))
	      (args (async-dired--rsync-get-args marks dest))
	      (buffer (async-dired--create-buffer "rsync"))
	      (process (apply 'start-file-process (buffer-name buffer)
			      buffer program args)))
    (add-to-list 'async-dired--ongoing-actions (cons process (current-buffer)))
    (async-dired--update-modeline process "0")
    (with-current-buffer buffer
      (erase-buffer)
      (insert (concat "Execute command: rsync " (mapconcat 'identity args " "))))
    (set-process-filter process 'async-dired--process-filter-copy)
    (set-process-sentinel process 'async-dired--process-sentinel)))

(defun async-dired--do-copy (old-fn &rest args)
  (if (executable-find async-dired--rsync-program)
      (async-dired--rsync)
    (apply old-fn args)))

;; delete

(defun async-dired--process-filter-delete (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (let ((beg (point))
	  lines progress)
      (insert str)
      (setq async-dired--current-count
	    (+ (count-lines beg (point-max))
	       async-dired--current-count))
      (setq progress (/ (* async-dired--current-count 100)
			async-dired--current-total))
      (async-dired--update-modeline process progress))))

(defun async-dired--delete-get-args (marks)
  (append (list "--force" "--recursive" "--verbose") marks))

(defun async-dired--delete (process)
  (let* ((program "rm")
	 (args (async-dired--delete-get-args async-dired--current-files))
	 (previous-process process)
	 (buffer (process-buffer previous-process))
	 (process (apply 'start-file-process (buffer-name buffer)
			 buffer program args)))
    (setcar (assoc previous-process async-dired--ongoing-actions) process)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Total files found: %s\n" async-dired--current-count))
      (insert (format "Execute command: rm %s\n"
		      (mapconcat 'identity args " "))))
    (set-process-filter process 'async-dired--process-filter-delete)
    (set-process-sentinel process 'async-dired--process-sentinel)))

(defun async-dired--get-total ()
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (count-lines (point) (point-max))))

(defun async-dired--process-filter-count (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

(defun async-dired--process-sentinel-count (process status)
  (let ((buffer (process-buffer process)))
    (if (zerop (process-exit-status process))
	(with-current-buffer buffer
	  (setq async-dired--current-total (async-dired--get-total))
	  (async-dired--delete process))
      (switch-to-buffer-other-window buffer)
      (async-dired--remove-action process))))

(defun async-dired--count-get-args (marks)
  (append (list "--all") marks))

(defun async-dired--count-files (marks)
  (when-let* ((files (mapcar #'untramp-path marks))
	      (program (executable-find async-dired--du-program))
	      (args (async-dired--count-get-args files))
	      (buffer (async-dired--create-buffer "delete"))
	      (process (apply 'start-file-process (buffer-name buffer)
			      buffer program args)))
    (add-to-list 'async-dired--ongoing-actions (cons process (current-buffer)))
    (async-dired--update-modeline process "0")
    (with-current-buffer buffer
      (setq async-dired--current-files files)
      (setq async-dired--current-count 0)
      (setq async-dired--current-total 0)
      (erase-buffer)
      (insert (format "Execute command: du %s\n"
		      (mapconcat 'identity args " "))))
    (set-process-filter process 'async-dired--process-filter-count)
    (set-process-sentinel process 'async-dired--process-sentinel-count)))

(defun async-dired--do-delete (old-fn &rest args)
  (if (executable-find async-dired--du-program)
      (async-dired--count-files (dired-get-marked-files))
    (apply old-fn args)))

(defun async-dired--do-flagged-delete (old-fn &rest args)
  (if (executable-find async-dired--du-program)
      (let* ((dired-marker-char dired-del-marker)
	     (regexp (dired-marker-regexp)))
	(when (save-excursion (goto-char (point-min))
			      (re-search-forward regexp nil t))
	  (async-dired--count-files (dired-map-over-marks
				     (dired-get-filename) nil))))
    (apply old-fn args)))

;;; External Functions

(defun async-dired-setup ()
  (advice-add 'dired-do-copy :around #'async-dired--do-copy)
  (advice-add 'dired-do-delete :around #'async-dired--do-delete)
  (advice-add 'dired-do-flagged-delete :around #'async-dired--do-flagged-delete))

(provide 'async-dired)
