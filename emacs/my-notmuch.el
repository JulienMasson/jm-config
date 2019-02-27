;;; my-notmuch.el --- Notmuch Configuration

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

;; load notmuch module
(require 'notmuch)

;; tree result format
(setq notmuch-tree-result-format `(("date" . "%12s  ")
				   ("authors" . "%-20s")
				   ((("tree" . "%s")("subject" . "%s")) ." %-54s ")))

;; refresh management
(defvar notmuch-hello-buffer "*notmuch-hello*")
(defvar notmuch-refresh-cmd "offlineimap")
(defvar notmuch-refresh-timer nil)
(defvar notmuch-refresh-every 60)

(defun notmuch-refresh-done (process state)
  (when (equal state "finished\n")
    (with-current-buffer notmuch-hello-buffer
      (notmuch-poll-and-refresh-this-buffer))))

(defun notmuch-refresh ()
  (let ((process (start-process-shell-command
		  "notmuch-refresh" nil notmuch-refresh-cmd)))
  (set-process-sentinel process #'notmuch-refresh-done)))

(defun notmuch-refresh-hook ()
  (setq notmuch-refresh-timer (run-at-time 1 notmuch-refresh-every 'notmuch-refresh)))
(add-hook 'notmuch-hello-mode-hook #'notmuch-refresh-hook)

(defun notmuch-refresh-cancel ()
  (when (string= (buffer-name (current-buffer)) notmuch-hello-buffer)
    (cancel-timer notmuch-refresh-timer)))
(advice-add 'notmuch-bury-or-kill-this-buffer :before #'notmuch-refresh-cancel)

;; show unread messages
(defun notmuch-show-unread ()
  (interactive)
  (notmuch-tree "tag:unread"))

(defun notmuch-thread-remove-unread ()
  (interactive)
  (notmuch-tree-tag-thread '("-unread"))
  (notmuch-refresh-this-buffer))

;; appply diff face in notmuch show mode
(defun notmuch-view-apply-diff-face (msg depth)
  (apply-minimal-diff-face-buffer))
(advice-add 'notmuch-show-insert-msg :after #'notmuch-view-apply-diff-face)

;; switch to buffer when RET on message
(defun notmuch-switch-to-buffer ()
  (switch-to-buffer-other-window notmuch-tree-message-buffer))
(advice-add 'notmuch-tree-show-message-in :after #'notmuch-switch-to-buffer)

;; use same buffer when display tree view
(defvar notmuch-tree-buffer-name "*notmuch-tree*")

(defun notmuch-tree (&optional query query-context target buffer-name open-target)
  (interactive)
  (if (null query)
      (setq query (notmuch-read-query "Notmuch tree view search: ")))
  (let ((buffer (get-buffer-create notmuch-tree-buffer-name))
	(inhibit-read-only t))
    (switch-to-buffer buffer)
    (erase-buffer))
  (set 'buffer-undo-list t)
  (notmuch-tree-worker query query-context target open-target)
  (setq truncate-lines t))

;; my show next/prev message func
(defun my-notmuch-show-next-message ()
  (interactive)
  (switch-to-buffer-other-window notmuch-tree-buffer-name)
  (notmuch-tree-next-message))

(defun my-notmuch-show-prev-message ()
  (interactive)
  (switch-to-buffer-other-window notmuch-tree-buffer-name)
  (notmuch-tree-prev-message))

;; maildir view in hello sections
(defun notmuch-maildir-assoc ()
  (let* ((maildir-path (notmuch-database-path))
	 (dirs (directory-files-recursively maildir-path "cur$" t))
	 (subdirs (mapcar (lambda (dir)
			    (replace-regexp-in-string
			     (format "%s/\\(.*\\)/cur"
				     (expand-file-name maildir-path))
			     "\\1" dir)) dirs))
	 maildir-assoc)
    (mapc (lambda (dir)
	    (cl-multiple-value-bind (account folder)
		(split-string dir "/")
	      (when folder
		(if (assoc account maildir-assoc)
		    (let ((folders (assoc-default account maildir-assoc)))
		      (setcdr (assoc account maildir-assoc)
			      (add-to-list 'folders folder t)))
		  (add-to-list 'maildir-assoc
			       (cons account `(,folder)) t)))))
	  subdirs)
    maildir-assoc))

(defun notmuch-count-query (query)
  (with-temp-buffer
    (insert query "\n")
    (unless (= (call-process-region (point-min) (point-max) notmuch-command
				    t t nil "count" "--batch") 0)
      (notmuch-logged-error "notmuch count --batch failed"))
    (goto-char (point-min))
    (read (current-buffer))))

(defun notmuch-insert-maildir (maildir)
  (widget-insert (propertize
		  (format "    * %s\n" (upcase (car maildir)))
		  'face 'font-lock-variable-name-face))
  (mapc (lambda (folder)
  	  (let* ((query (format "folder:\"%s/%s\"" (car maildir) folder))
  		 (total (notmuch-count-query query))
  		 (unread (notmuch-count-query
  			  (concat query " and tag:unread")))
  		 (widget-push-button-prefix "")
  		 (widget-push-button-suffix ""))
  	    (widget-insert "      ")
  	    (widget-create 'push-button
  			   :notify #'notmuch-hello-widget-search
  			   :notmuch-search-terms query
  			   :notmuch-search-oldest-first 'newest-first
  			   :notmuch-search-type 'tree
  			   (format "%-20s" (replace-regexp-in-string
  					    "^.*\\." "" folder)))
  	    (widget-insert (propertize
  			    (format "(%s/%s)\n" unread total)
  			    'face (if (> unread 0)
  				      'mu4e-unread-face
  				    'mu4e-header-face)))))
  	(cdr maildir))
  (widget-insert "\n"))

(defun notmuch-insert-maildir-header ()
  (widget-insert "\n")
  (widget-insert (propertize "  Accounts\n\n" 'face 'mu4e-title-face)))

(defun notmuch-hello-maildir ()
  (let ((maildirs (notmuch-maildir-assoc)))
    (notmuch-insert-maildir-header)
    (mapcar #'notmuch-insert-maildir maildirs)))

(setq notmuch-hello-sections (list #'notmuch-hello-maildir))


(provide 'my-notmuch)
