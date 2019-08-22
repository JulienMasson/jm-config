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

(defgroup send-patch nil
  "Send patch."
  :group 'mail)

;;; Customization

(defcustom send-patch-compose-hook nil
  "Hook run when composing patch mail."
  :type 'hook
  :group 'send-patch)

(defcustom send-patch-get-range-funcs
  '(("Remote Head" . send-patch-get-range-from-remote-head)
    ("Last Tag"    . send-patch-get-range-from-last-tag)
    ("Range"       . (lambda () (read-string "Range: "))))
  "Alist of functions used to get range when creating patchs"
  :type 'alist
  :group 'send-patch)

(defcustom send-patch-get-recipients-funcs
  '(("Empty" . send-patch-get-recipients-empty))
  "Alist of functions used to fill recipients for each patch"
  :type 'alist
  :group 'send-patch)

;;; Internal Variables

(defvar patch-mail-buffers nil)

(defvar cover-letter-message-id nil)

(defvar auto-send-patch nil)

;;; Internal Functions

(defun send-patch-get-range-from-last-tag ()
  (format "%s..HEAD" (magit-git-string "describe" "--abbrev=0" "--tags")))

(defun send-patch-get-range-from-remote-head ()
  (format "%s..HEAD" (magit-get-upstream-ref)))

(defun send-patch-get-range (range)
  (interactive (list (completing-read "From: "
				      (mapcar #'car send-patch-get-range-funcs))))
  (if (assoc range send-patch-get-range-funcs)
      (funcall (assoc-default range send-patch-get-range-funcs))
    (error (format "Unknown %s range function" range))))

(defun send-patch-get-recipients-empty (patchs)
  (list nil nil))

(defun send-patch-get-recipients (patchs)
  (let ((recip (completing-read "Fill Recipients: "
				(mapcar #'car send-patch-get-recipients-funcs))))
    (if (assoc recip send-patch-get-recipients-funcs)
	(funcall (assoc-default recip send-patch-get-recipients-funcs) patchs)
      (error (format "Unknown %s recipients function" recip)))))

(defun message-position-on (header afters)
  (interactive)
  (push-mark)
  (message-position-on-field header afters))

(defmacro foreach-send-patchs (&rest body)
  `(let ((inhibit-read-only t))
     (dolist (patch patch-mail-buffers)
       (with-current-buffer patch
	 ,@body
	 (set-buffer-modified-p nil)))))

(defun send-patch-update-fields ()
  (interactive)
  (let ((to (message-fetch-field "To"))
	(cc (message-fetch-field "Cc")))
    (foreach-send-patchs
     (cl-flet ((goto-begin ()
	         (message-beginning-of-header visual-line-mode)
	         (delete-region (point) (line-end-position))))
       (message-position-on "To" "From")
       (goto-begin)
       (if to (insert to))
       (message-position-on "Cc" "To")
       (goto-begin)
       (if cc (insert cc))))))

(defun send-patch-all ()
  (interactive)
  (setq auto-send-patch t)
  (message-send-and-exit))

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

    ;; define custom keybindings
    (define-key message-mode-map (kbd "C-c C-k")
      'send-patch-kill-buffer)
    (define-key message-mode-map (kbd "C-c C-r")
      'send-patch-update-fields)
    (define-key message-mode-map (kbd "C-c C-p")
      'send-patch-all)))

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

	  ;; add In-Reply-To and References header
	  ;; if we have a cover letter
	  (when cover-letter-message-id
	    (message-position-on "In-Reply-To" "Subject")
	    (insert cover-letter-message-id)
	    (message-position-on "References" "In-Reply-To")
	    (insert cover-letter-message-id))

	  (set-buffer-modified-p nil)
	  (if auto-send-patch
	      (message-send-and-exit)
	    (switch-to-buffer mail-buffer)))
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
  (setq auto-send-patch nil)
  (mapc #'delete-file (file-expand-wildcards "*.patch"))
  (when patch-mail-buffers
    (mapc #'kill-buffer patch-mail-buffers)
    (setq patch-mail-buffers nil))
  (setq cover-letter-message-id nil)
  (remove-hook 'message-header-hook 'get-cover-letter-message-id)
  (remove-hook 'message-sent-hook 'process-patch-mails))

;;; External Functions

(defun send-patch (&optional version)
  (interactive)
  (let* ((default-directory (magit-toplevel))
	 (range (call-interactively 'send-patch-get-range))
	 (count (length (magit-git-lines "log" "--oneline" range)))
	 patchs)
    (unless (zerop count)
      (send-patch-cleanup-env)
      (magit-run-git "format-patch" range
		     (if version (format "-v%d" version))
		     (if (> count 1) "--cover-letter"))
      (setq patchs (file-expand-wildcards "*.patch"))
      (cl-multiple-value-bind (to cc)
	  (send-patch-get-recipients patchs)
	(setq patch-mail-buffers (generate-patch-mails patchs to cc)))
      (process-patch-mails)
      (when (> count 1)
	(add-hook 'message-header-hook 'get-cover-letter-message-id)))))

(defun send-patch-new-version (version)
  (interactive "nSend Patch version: ")
  (send-patch version))

(provide 'send-patch)
