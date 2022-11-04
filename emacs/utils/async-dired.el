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

;;; Internal Functions

(defun async-dired--create-buffer (action)
  (let* ((name (format "*async-dired-%s*" action))
	 (buffer (generate-new-buffer-name name)))
    (get-buffer-create buffer)))

(defun async-dired--find-buffer (dest)
  (let ((dest (file-name-directory (expand-file-name dest))))
    (cl-find-if (lambda (buffer)
                  (with-current-buffer buffer
                    (and (string= major-mode "dired-mode")
                         (string= dest (expand-file-name default-directory)))))
                (buffer-list))))

(defun async-dired--process-filter (process str)
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (string-join (split-string str "") "\n"))))))

(defun async-dired--process-sentinel (from dest done-cb process status)
  (let ((buffer (process-buffer process)))
    (with-current-buffer from
      (dired-sort-set-modeline)
      (when done-cb
        (funcall done-cb)))
    (if (zerop (process-exit-status process))
        (progn
          (kill-buffer buffer)
          (when dest
            (with-current-buffer dest
              (revert-buffer))))
      (switch-to-buffer-other-window buffer))))

(defun async-dired--get-marks-dest ()
  (when-let* ((marks (dired-get-marked-files))
              (dest (async-dired--rsync-prompt marks)))
    (unless (string-suffix-p "/" dest)
      (setq marks (mapcar (lambda (mark)
                            (if (file-directory-p mark)
                                (file-name-as-directory mark)
                              mark))
                          marks)))
    (list marks dest)))

;; delete
(defun dired-get-delete-files ()
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
         files)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (dired-get-filename) files)))
    (unless files
      (setq files (list (dired-get-filename))))
    files))

(defun async-dired--delete (marks)
  (when-let* ((files (mapcar #'untramp-path marks))
	      (args (append (list "-rf") files))
	      (buffer (async-dired--create-buffer "delete"))
	      (process (apply 'start-file-process (buffer-name buffer)
			      buffer "rm" args)))
    (setq mode-name "Dired remove")
    (force-mode-line-update)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Execute command: rm %s\n" (string-join args " "))))
    (set-process-filter process 'async-dired--process-filter)
    (set-process-sentinel process (apply-partially #'async-dired--process-sentinel
                                                   (current-buffer) (current-buffer) nil))))

(defun async-dired--do-delete (&optional arg)
  (async-dired--delete (dired-get-delete-files)))

;; rsync
(defun async-dired--rsync-prompt (marks)
  (let* ((nbr (length marks))
	 (msg (propertize (if (> nbr 1)
			      (format "* [ %d files ]" nbr)
			    (file-name-nondirectory (car marks)))
			  'face 'warning))
         (dir (read-directory-name (format "Rsync %s to: " msg))))
    (expand-file-name dir)))

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

(defun async-dired--rsync (name marks dest &optional extra-args done-cb)
  (let* ((dest-buffer (async-dired--find-buffer dest))
	 (default-directory (async-dired--rsync-default-directory marks dest))
	 (program (executable-find async-dired--rsync-program))
	 (args (async-dired--rsync-get-args marks dest extra-args))
	 (buffer (async-dired--create-buffer name))
	 (process (apply 'start-file-process (buffer-name buffer)
			 buffer program args)))
    (setq mode-name (concat "Dired " name))
    (force-mode-line-update)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Execute command: rsync %s\n" (string-join args " "))))
    (set-process-filter process 'async-dired--process-filter)
    (set-process-sentinel process (apply-partially #'async-dired--process-sentinel
                                                   (current-buffer) dest-buffer done-cb))))

;; copy
(defun async-dired--do-copy (old-fn &rest args)
  (if (executable-find async-dired--rsync-program)
      (pcase-let ((`(,marks ,dest) (async-dired--get-marks-dest)))
        (async-dired--rsync "copy" marks dest))
    (apply old-fn args)))

;; rename
(defun async-dired--do-rename (old-fn &rest args)
  (if (executable-find async-dired--rsync-program)
      (pcase-let ((`(,marks ,dest) (async-dired--get-marks-dest)))
        (async-dired--rsync "rename" marks dest
                            (list "--remove-source-files")
                            `(lambda () (async-dired--delete (list ,@marks)))))
    (apply old-fn args)))

;;; External Functions

(defun async-dired-setup ()
  (advice-add 'dired-do-copy :around #'async-dired--do-copy)
  (advice-add 'dired-do-rename :override #'async-dired--do-rename)
  (advice-add 'dired-do-delete :override #'async-dired--do-delete)
  (advice-add 'dired-do-flagged-delete :override #'async-dired--do-delete))

(provide 'async-dired)
