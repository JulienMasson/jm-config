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

;;; Internal Functions

(defun send-patch-get-range-from-last-tag ()
  (format "%s..HEAD" (magit-git-string "describe" "--abbrev=0" "--tags")))

(defun send-patch-get-range-from-remote-head ()
  (format "%s..HEAD" (magit-get-upstream-ref)))

(defun send-patch-get-range (range)
  (interactive (list (completing-read "From: " (mapcar #'car send-patch-get-range-funcs))))
  (if-let ((func (assoc-default range send-patch-get-range-funcs)))
      (funcall func)
    (error (format "Unknown %s range function" range))))

(defun send-patch-get-recipients-empty (patch)
  (list nil nil))

(defun send-patch-get-recipients-func ()
  (let* ((recipients (mapcar #'car send-patch-get-recipients-funcs))
         (recip (completing-read "Fill Recipients: " recipients)))
    (assoc-default recip send-patch-get-recipients-funcs)))

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

(defun send-patch-goto-begin ()
  (let ((separator (concat "^" (regexp-quote mail-header-separator) "$")))
    (goto-char (point-max))
    (re-search-backward separator nil t)))

(defun send-patch-toggle-read-only ()
  (interactive)
  (save-excursion
    (when (send-patch-goto-begin)
      (let ((prop (get-text-property (point) 'read-only))
            (inhibit-read-only t))
        (add-text-properties (1- (point)) (point-max) `(read-only ,(not prop)))))))

(defun send-patch-compose-mail (patch to cc)
  (with-current-buffer (get-buffer-create (format "<%s>" patch))
    (erase-buffer)

    ;; patch content
    (insert-file-contents patch)

    ;; remove everything before From header
    (goto-char (point-max))
    (while (re-search-backward "^From:" nil t))
    (delete-region (point) (point-min))

    ;; seperator
    (mail-sendmail-delimit-header)

    ;; To and Cc header
    (message-position-on "To" "From")
    (when to (insert (string-join to ", ")))
    (message-position-on "Cc" "To")
    (when cc (insert (string-join cc ", ")))

    ;; message mode
    (goto-char (point-min))
    (message-mode)

    ;; run compose hook
    (run-hooks 'send-patch-compose-hook)

    ;; remove message forbidden props hook
    (remove-hook 'after-change-functions #'message-strip-forbidden-properties t)

    ;; custom keybindings
    (local-set-key (kbd "C-c C-c") 'send-patch-all)
    (local-set-key (kbd "C-c C-k") 'send-patch-cancel)
    (local-set-key (kbd "C-c C-r") 'send-patch-update-fields)
    (local-set-key (kbd "C-c C-t") 'send-patch-toggle-read-only)

    ;; send patchs one after another
    (add-hook 'message-sent-hook 'send-patch-all nil t)

    (set-buffer-modified-p nil)
    (current-buffer)))

(defun get-cover-letter-message-id ()
  (setq cover-letter-message-id (message-fetch-field "Message-ID")))

(defun generate-cover-letter (patch to cc)
  (with-current-buffer (send-patch-compose-mail patch to cc)
    (add-hook 'message-header-hook 'get-cover-letter-message-id nil t)
    (current-buffer)))

(defun generate-patch (patch to cc)
  (with-current-buffer (send-patch-compose-mail patch to cc)
    (save-excursion
      (when (send-patch-goto-begin)
        (add-text-properties (1- (point)) (point-max) '(read-only t))))
    (set-buffer-modified-p nil)
    (current-buffer)))

(defun send-patch-all ()
  (interactive)
  (if-let ((buffer (pop patch-mail-buffers)))
      (with-current-buffer buffer
	;; set date to current time
	(message-position-on "Date" "Cc")
	(message-beginning-of-line)
	(delete-region (point) (line-end-position))
	(insert (format-time-string "%a, %e %b %Y %T %z" (current-time)))

	;; add In-Reply-To and References header
	;; if we have a cover letter
	(when cover-letter-message-id
	  (message-position-on "In-Reply-To" "Subject")
	  (insert cover-letter-message-id)
	  (message-position-on "References" "In-Reply-To")
	  (insert cover-letter-message-id))

	(set-buffer-modified-p nil)
	(message-send-and-exit))
    (send-patch-cleanup-env)))

(defun send-patch-cancel ()
  (interactive)
  (send-patch-cleanup-env))

(defun send-patch-cleanup-env ()
  (mapc #'delete-file (file-expand-wildcards "*.patch"))
  (when patch-mail-buffers
    (mapc #'kill-buffer patch-mail-buffers)
    (setq patch-mail-buffers nil))
  (setq cover-letter-message-id nil))

;;; External Functions

(defun send-patch (&optional version)
  (interactive)
  (when-let* ((default-directory (magit-toplevel))
	      (range (call-interactively 'send-patch-get-range))
	      (count (length (magit-git-lines "log" "--oneline" range)))
              (recip-func (send-patch-get-recipients-func)))
    (unless (zerop count)
      (send-patch-cleanup-env)
      (magit-run-git "format-patch" range
		     (if version (format "-v%d" version))
		     (if (> count 1) "--cover-letter"))
      (when-let ((patchs (file-expand-wildcards "*.patch")))
        ;; cover letter
        (when (> count 1)
          (let ((cover-letter (pop patchs)))
            (pcase-let ((`(,to ,cc) (funcall recip-func cover-letter)))
              (add-to-list 'patch-mail-buffers (generate-cover-letter cover-letter to cc) t))))
        ;; patchs
        (dolist (patch patchs)
          (pcase-let ((`(,to ,cc) (funcall recip-func patch)))
	    (add-to-list 'patch-mail-buffers (generate-patch patch to cc) t)))
        (switch-to-buffer (car patch-mail-buffers))))))

(defun send-patch-new-version (version)
  (interactive "nSend Patch version: ")
  (send-patch version))

(provide 'send-patch)
