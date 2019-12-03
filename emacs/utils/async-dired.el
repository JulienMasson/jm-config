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

(defvar-local async-dired--files nil)

(defvar-local async-dired--count nil)

(defvar-local async-dired--total nil)

(defvar-local async-dired--revert-path nil)

(defvar-local async-dired--done-cb nil)

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

(defun async-dired--reset-modeline (process)
  (with-async-dired-buffer process
    (dired-sort-set-modeline)))

(defun async-dired--create-buffer (action)
  (let* ((name (format "*async-dired-%s*" action))
	 (buffer (generate-new-buffer-name name)))
    (get-buffer-create buffer)))

(defun async-dired--assoc-dired-buffers ()
  (delq nil (mapcar (lambda (buffer)
		      (with-current-buffer buffer
			(when (string= major-mode "dired-mode")
			  (cons (expand-file-name default-directory)
				buffer))))
		    (buffer-list))))

(defun async-dired--revert-buffers ()
  (let ((dired-buffers (async-dired--assoc-dired-buffers)))
    (mapc (lambda (path)
	    (when-let* ((expand-path (expand-file-name path))
			(buffer (assoc-default expand-path dired-buffers)))
	      (with-current-buffer buffer
		(revert-buffer))))
	  async-dired--revert-path)))

(defun async-dired--process-sentinel (process status)
  (async-dired--reset-modeline process)
  (async-dired--remove-action process)
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (if (zerop (process-exit-status process))
	  (with-current-buffer buffer
	    (when async-dired--done-cb
	      (funcall async-dired--done-cb))
	    (async-dired--revert-buffers)
	    (kill-buffer))
	(switch-to-buffer-other-window buffer)))))

;; rsync
(defun async-dired--rsync-get-progress (output)
  (when-let* ((lines (split-string output "\n"))
	      (line (last lines))
	      (regexp " \\([0-9]+\\)%"))
    (when (string-match regexp str)
      (match-string 1 str))))

(defun async-dired--process-filter-rsync (process str)
  (let* ((output (replace-regexp-in-string "" "\n" str))
	 (progress (async-dired--rsync-get-progress output)))
    (with-current-buffer (process-buffer process)
      (save-excursion
	(goto-char (point-max))
	(insert output)
	(when progress
	  (async-dired--update-modeline process progress))))))

(defun async-dired--rsync-prompt (marks)
  (let* ((nbr (length marks))
	 (msg (propertize (if (> nbr 1)
			      (format "* [ %d files ]" nbr)
			    (file-name-nondirectory (car marks)))
			 'face 'warning)))
    (expand-file-name (read-directory-name (format "Rsync %s to: " msg)))))

(defun async-dired--rsync-get-args (marks dest &optional extra-args)
  (let ((default-args (list "--archive" "--info=progress2"))
	(src-remote-p (tramp-tramp-file-p (car marks)))
	(dest-remote-p (tramp-tramp-file-p dest)))
    (append default-args (if extra-args extra-args)
	    (cond
	     ;; src and dest on local
	     ((not (or src-remote-p dest-remote-p))
	      (append marks (list dest)))
	     ;; src and dest on remote
	     ((and src-remote-p dest-remote-p)
	      (mapcar #'untramp-path (append marks (list dest))))
	     ;; src or dest is on remote, the other on local
	     (t (mapcar (lambda (elem)
			  (replace-regexp-in-string "/ssh:" "" elem))
			(append marks (list dest))))))))

(defun async-dired--rsync-default-directory (marks dest)
  (let ((src-remote-p (tramp-tramp-file-p (car marks)))
	(dest-remote-p (tramp-tramp-file-p dest)))
    (if (and src-remote-p dest-remote-p)
	dest
      (getenv "HOME"))))

(defun async-dired--rsync (name &optional extra-args done-cb)
  (when-let* ((current-dir default-directory)
	      (marks (dired-get-marked-files))
	      (files (mapcar #'untramp-path marks))
	      (dest (async-dired--rsync-prompt marks))
	      (default-directory (async-dired--rsync-default-directory marks dest))
	      (program (executable-find async-dired--rsync-program))
	      (args (async-dired--rsync-get-args marks dest extra-args))
	      (buffer (async-dired--create-buffer name))
	      (process (apply 'start-file-process (buffer-name buffer)
			      buffer program args)))
    (add-to-list 'async-dired--ongoing-actions (cons process (current-buffer)))
    (async-dired--update-modeline process "0")
    (with-current-buffer buffer
      (setq async-dired--files files)
      (setq async-dired--revert-path (list dest current-dir))
      (setq async-dired--done-cb done-cb)
      (erase-buffer)
      (insert (concat "Execute command: rsync " (mapconcat 'identity args " "))))
    (set-process-filter process 'async-dired--process-filter-rsync)
    (set-process-sentinel process 'async-dired--process-sentinel)))

;; copy
(defun async-dired--do-copy (old-fn &rest args)
  (if (executable-find async-dired--rsync-program)
      (async-dired--rsync "copy")
    (apply old-fn args)))

;; rename
(defun async-dired--delete-directories ()
  (mapc (lambda (file)
	  (when (file-directory-p file)
	    (delete-directory file t)))
	async-dired--files))

(defun async-dired--do-rename (old-fn &rest args)
  (if (executable-find async-dired--rsync-program)
      (async-dired--rsync "rename" (list "--remove-source-files")
			  #'async-dired--delete-directories)
    (apply old-fn args)))

;; delete
(defun async-dired--process-filter-delete (process str)
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (let ((beg (point))
	    lines progress)
	(insert str)
	(setq async-dired--count
	      (+ (count-lines beg (point-max))
		 async-dired--count))
	(setq progress (/ (* async-dired--count 100)
			  async-dired--total))
	(async-dired--update-modeline process progress)))))

(defun async-dired--delete-get-args (marks)
  (append (list "--force" "--recursive" "--verbose") marks))

(defun async-dired--delete (process)
  (let* ((program "rm")
	 (args (async-dired--delete-get-args async-dired--files))
	 (previous-process process)
	 (buffer (process-buffer previous-process))
	 (process (apply 'start-file-process (buffer-name buffer)
			 buffer program args)))
    (setcar (assoc previous-process async-dired--ongoing-actions) process)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Total files found: %s\n" async-dired--count))
      (insert (format "Execute command: rm %s\n"
		      (mapconcat 'identity args " "))))
    (set-process-filter process 'async-dired--process-filter-delete)
    (set-process-sentinel process 'async-dired--process-sentinel)))

(defun async-dired--get-total ()
  (string-to-number (buffer-substring (line-beginning-position)
				      (line-end-position))))

(defun async-dired--process-filter-count (process str)
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert str))))

(defun async-dired--process-sentinel-count (process status)
  (let ((buffer (process-buffer process)))
    (if (zerop (process-exit-status process))
	(with-current-buffer buffer
	  (setq async-dired--total (async-dired--get-total))
	  (async-dired--delete process))
      (when (buffer-live-p buffer)
	(switch-to-buffer-other-window buffer))
      (async-dired--remove-action process))))

(defun async-dired--count-command (files)
  (format "ls -fR %s | wc -l"
	  (mapconcat 'identity files " ")))

(defun async-dired--count-files (marks)
  (when-let* ((dired-buffer (current-buffer))
	      (files (mapcar #'untramp-path marks))
	      (command (async-dired--count-command files))
	      (buffer (async-dired--create-buffer "delete"))
	      (process (start-file-process-shell-command
			(buffer-name buffer) buffer command)))
    (add-to-list 'async-dired--ongoing-actions (cons process (current-buffer)))
    (async-dired--update-modeline process "0")
    (with-current-buffer buffer
      (setq async-dired--files files)
      (setq async-dired--count 0)
      (setq async-dired--total 0)
      (setq async-dired--revert-path (list default-directory))
      (erase-buffer)
      (insert (format "Execute command: du %s\n" command)))
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
  (advice-add 'dired-do-rename :around #'async-dired--do-rename)
  (advice-add 'dired-do-delete :around #'async-dired--do-delete)
  (advice-add 'dired-do-flagged-delete :around #'async-dired--do-flagged-delete))

(provide 'async-dired)
