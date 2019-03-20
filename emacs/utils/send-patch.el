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

(defun message-position-on (header afters)
  (interactive)
  (push-mark)
  (message-position-on-field header afters))

(defun get-kernel-address-components (patchs)
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

(defun get-uboot-address-components (patchs)
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

    ;; insert cc and to header
    (message-position-on "To" "From")
    (insert (mapconcat 'identity to ", "))
    (message-position-on "Cc" "To")
    (insert (mapconcat 'identity cc ", "))

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

(defun send-patch (&optional version)
  (interactive)
  (let* ((default-directory (magit-toplevel))
	 (last-tag (magit-git-string "describe" "--abbrev=0" "--tags"))
	 (range (format "%s..HEAD" last-tag))
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
	  (get-kernel-address-components patchs)
	(setq patch-mail-buffers (generate-patch-mails patchs to cc)))
      (process-patch-mails)
      (when (> count 1)
	(add-hook 'message-header-hook 'get-cover-letter-message-id)))))

(defun uboot-patch-send (&optional version)
  (interactive)
  (let* ((default-directory (magit-toplevel))
	 (remote-head (magit-get-upstream-ref))
	 (range (format "%s..HEAD" remote-head))
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
	  (get-uboot-address-components patchs)
	(setq patch-mail-buffers (generate-patch-mails patchs to cc)))
      (process-patch-mails)
      (when (> count 1)
	(add-hook 'message-header-hook 'get-cover-letter-message-id)))))

(defun uboot-patch-new-version (version)
  (interactive "nU-Boot Patch version: ")
  (uboot-patch-send version))

(defun send-patch-new-version (version)
  (interactive "nSend Patch version: ")
  (send-patch version))


(provide 'send-patch)
