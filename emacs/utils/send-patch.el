;;; send-patch.el --- Send Patch Utils

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

(require 'magit)
(require 'message)
(require 'sendmail)

(defcustom send-patch-compose-hook nil
  "Hook run when composing patch mail."
  :group 'message-various
  :type 'hook)

(defcustom send-patch-user-email nil
  "User Email added in Cc."
  :type 'string
  :safe 'stringp)

;; internal variables
(defvar patch-mail-buffers nil)
(defvar cover-letter-message-id nil)
(defvar send-patch-backends nil)

(cl-defstruct send-patch
  (name        nil :read-only t :type 'string)
  (url         nil :read-only t :type 'string)
  (get-range   nil :read-only t :type 'function)
  (get-address nil :read-only t :type 'function))

(defun message-position-on (header afters)
  (interactive)
  (push-mark)
  (message-position-on-field header afters))

(defun send-patch-get-range-from-last-tag ()
  (format "%s..HEAD" (magit-git-string "describe" "--abbrev=0" "--tags")))

(defun send-patch-get-range-from-remote-head ()
  (format "%s..HEAD" (magit-get-upstream-ref)))

(defun get-address-kernel (patchs)
  (let* ((files-str (mapconcat 'identity patchs " "))
	 (cmd (concat "./scripts/get_maintainer.pl " files-str))
	 (user-list (split-string
		     (shell-command-to-string cmd) "\n"))
	 (cc `("linux-kernel@vger.kernel.org"
	       ,(if send-patch-user-email
		    send-patch-user-email)))
	 to)
    (mapc (lambda (user)
	    (save-match-data
	      (when (string-match "\\(.*\\) \(\\(.*\\):.*" user)
		(if (string-equal (match-string 2 user)
				  "maintainer")
		    (add-to-list 'to (match-string 1 user))
		  (add-to-list 'cc (match-string 1 user))))))
	  user-list)
    (list to cc)))

(defun get-address-uboot (patchs)
  (let* ((files-str (mapconcat 'identity patchs " "))
	 (cmd (concat "./scripts/get_maintainer.pl " files-str))
	 (user-list (split-string
		     (shell-command-to-string cmd) "\n"))
	 (to '("u-boot@lists.denx.de"))
	 cc)
    (mapc (lambda (user)
	    (save-match-data
	      (when (string-match "\\(.*\\) \(\\(.*\\):.*" user)
		(if (string-equal (match-string 2 user)
				  "maintainer")
		    (add-to-list 'cc (match-string 1 user))
		  (add-to-list 'to (match-string 1 user))))))
	  user-list)
    (list to cc)))

(defun send-patch-compose-mail (mail-buf patch to cc)
  (with-current-buffer mail-buf
    (erase-buffer)

    ;; insert patch content
    (insert-file-contents patch)

    ;; remove everything before From header
    (goto-char (point-max))
    (while (re-search-backward "^From:" nil t))
    (delete-region (point) (point-min))

    ;; insert seperator
    (mail-sendmail-delimit-header)

    ;; insert To and Cc header
    (message-position-on "To" "From")
    (when to
      (insert (mapconcat 'identity to ", ")))
    (message-position-on "Cc" "To")
    (when cc
      (insert (mapconcat 'identity cc ", ")))

    ;; apply message mode
    (goto-char (point-min))
    (message-mode)

    ;; run compose hook
    (run-hooks 'send-patch-compose-hook)

    ;; set buffer read only and not modified
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)

    ;; redefine keybinding `C-c C-k'
    (define-key message-mode-map (kbd "C-c C-k")
      'send-patch-kill-buffer)))

(defun generate-patch-mails (patchs to cc)
  (mapcar (lambda (patch)
	    (let ((mail-buf (get-buffer-create
			     (format "<mail-%s>" patch))))
	      (send-patch-compose-mail mail-buf patch to cc)
	      mail-buf))
	  patchs))

(defun process-patch-mails ()
  (let ((mail-buffer (pop patch-mail-buffers)))
    (if mail-buffer
	(with-current-buffer mail-buffer
	  (setq buffer-read-only nil)

	  ;; set date to current time
	  (message-position-on "Date" "Cc")
	  (message-beginning-of-line)
	  (delete-region (point) (line-end-position))
	  (insert (format-time-string
		   "%a, %e %b %Y %T %z" (current-time)))

	  ;; add Reply-To and References header
	  ;; if we have a cover letter
	  (when cover-letter-message-id
	    (message-goto-reply-to)
	    (insert cover-letter-message-id)
	    (message-position-on "References" "Reply-To")
	    (insert cover-letter-message-id))

	  (set-buffer-modified-p nil)
	  (switch-to-buffer mail-buffer))
      (send-patch-cleanup-env))))

(defun get-cover-letter-message-id ()
  (let ((message-id (message-fetch-field "Message-ID")))
    (setq cover-letter-message-id message-id)
    (remove-hook 'message-header-hook 'get-cover-letter-message-id)
    (add-hook 'message-sent-hook 'process-patch-mails)))

(defun send-patch-kill-buffer ()
  (interactive)
  (kill-current-buffer)
  (send-patch-cleanup-env))

(defun send-patch-cleanup-env ()
  (when patch-mail-buffers
    (mapc #'kill-buffer patch-mail-buffers)
    (setq patch-mail-buffers nil))
  (setq cover-letter-message-id nil)
  (remove-hook 'message-header-hook 'get-cover-letter-message-id)
  (remove-hook 'message-sent-hook 'process-patch-mails))

(defun send-patch-find-backend-by-url (url)
  (seq-find (lambda (backend)
	      (string= (send-patch-url backend) url))
	    send-patch-backends))

(defun send-patch-find-backend-by-name (name)
  (seq-find (lambda (backend)
	      (string= (send-patch-name backend) name))
	    send-patch-backends))

(defun send-patch-prompt-backend ()
  (let* ((names (mapcar 'send-patch-name send-patch-backends))
	 (choice (completing-read "Select Backend: " names)))
    (send-patch-find-backend-by-name choice)))

(defun send-patch-find-backend ()
  (let* ((remote (magit-get-remote))
	 (remote-url (magit-get "remote" remote "url"))
	 (backend (send-patch-find-backend-by-url remote-url)))
    (unless backend
      (setq backend (send-patch-prompt-backend)))
    backend))

(defun send-patch (&optional version)
  (interactive)
  (let* ((default-directory (magit-toplevel))
	 (backend (send-patch-find-backend))
	 (range (funcall (send-patch-get-range backend)))
	 	 (count (length (magit-git-lines "log" "--oneline" range)))
	 patchs)
    (unless (zerop count)
      (send-patch-cleanup-env)
      (mapc #'delete-file (file-expand-wildcards "*.patch"))
      (magit-run-git "format-patch" range
		     (if version (format "-v%d" version))
		     (if (> count 1) "--cover-letter"))
      (setq patchs (file-expand-wildcards "*.patch"))
      (cl-multiple-value-bind (to cc)
	  (funcall (send-patch-get-address backend) patchs)
	(setq patch-mail-buffers (generate-patch-mails patchs to cc)))
      (process-patch-mails)
      (when (> count 1)
	(add-hook 'message-header-hook 'get-cover-letter-message-id)))))

(defun send-patch-new-version (version)
  (interactive "nSend Patch version: ")
  (send-patch version))

(defun send-patch-backend-eq (a1 a2)
  (when (and (send-patch-p a1) (send-patch-p a2))
    (or (string= (send-patch-name a1) (send-patch-name a2))
	(string= (send-patch-url a1) (send-patch-url a2)))))

(cl-defun send-patch-register-backend (&key name url get-range get-address)
  (add-to-list 'send-patch-backends
	       (make-send-patch :name name
				:url url
				:get-range get-range
				:get-address get-address)
	       nil 'send-patch-backend-eq))

;; add Interactive backend by default
(defun send-patch-prompt-range ()
  (let* ((assoc-range '(("Remote Head" . send-patch-get-range-from-remote-head)
			("Last Tag" . send-patch-get-range-from-last-tag)))
	 (choice (completing-read "From: " (mapcar #'car assoc-range))))
    (funcall (assoc-default choice assoc-range))))

(defun send-patch-stub-get-address (patchs)
  (list nil nil))

(send-patch-register-backend :name "Interactive"
			     :url "Unknown URL Remote"
			     :get-range 'send-patch-prompt-range
			     :get-address 'send-patch-stub-get-address)


(provide 'send-patch)
