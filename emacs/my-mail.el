;;; my-mail.el --- Mail Configuration

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

;; default message from style
(setq message-from-style 'angles)

;; default citation format
(setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; message buffer will be killed after sending a message
(setq message-kill-buffer-on-exit t)

;; default config when sending mail
(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail
      smtpmail-debug-info nil
      mail-setup-hook nil
      sendmail-program (executable-find "msmtp"))

;; utils to get a mail agent function
(defun get-mail-agent-function (string)
  (let ((backend (replace-regexp-in-string
		  "-user-agent" ""
		  (symbol-name mail-user-agent))))
    (intern (concat backend string))))

;; mail client
(defun mail-client ()
  (interactive)
  (let ((fun (get-mail-agent-function "")))
    (when (symbol-function fun)
	(funcall fun))))

;; accounts management
(defvar mail-accounts-alist
  '(("Gmail"
     (user-mail-address "massonju.eseo@gmail.com")
     (user-full-name "Julien Massson")
     (message-sendmail-extra-arguments ("-a" "perso")))))

(defun mail-set-vars (account)
  (mapc (lambda (var)
	  (set (car var) (cadr var)))
	(assoc-default account mail-accounts-alist)))

(defun mail-compose-new (account)
  (interactive (list (completing-read "Compose with account: "
				      (mapcar #'car mail-accounts-alist))))
  (let ((fun (get-mail-agent-function "-compose-new")))
    (mail-set-vars account)
    (if (symbol-function fun)
	(funcall fun)
      (compose-mail))))

;; org msg
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil \\n:t")
(setq org-msg-startup "hidestars noindent inlineimages")
(setq org-msg-greeting-fmt "\nHi %s,\n\n")
(setq org-msg-greeting-fmt-mailto t)
(setq org-msg-signature "

Regards,

#+begin_signature
--\n
Julien Masson
#+end_signature")
(org-msg-mode)

;; apply minimal diff face
(defun apply-minimal-diff-face-buffer ()
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward "^diff \-\-git" nil t))
    (while (not (eobp))
      (let* ((start (point))
	     (end (line-end-position))
	     (str (buffer-substring-no-properties start end))
	     (inhibit-read-only t))
	(cond ((string-match "^\\(---\\|\\+\\+\\+\\)" str)
	       (add-face-text-property start end 'diff-file-header))
	      ((string-match "^@@" str)
	       (add-face-text-property start end 'diff-header))
	      ((string-match "^\\+" str)
	       (add-face-text-property start end 'diff-added))
	      ((string-match "^\\-" str)
	       (add-face-text-property start end 'diff-removed)))
	(forward-line)))))

;; apply diff face in message with font-lock keywords
(defun diff-font-lock-make-header-matcher (regexp)
  (let ((form `(lambda (limit)
		 (let (diff-start)
		   (save-excursion
		     (goto-char (point-max))
		     (while (re-search-backward "^diff \-\-git" nil t))
		     (setq diff-start (point)))
		   (and (> (point) diff-start)
			(re-search-forward ,regexp limit t))))))
    (if (featurep 'bytecomp)
	(byte-compile form)
      form)))

(defvar my-diff-font-lock-keywords `((,(diff-font-lock-make-header-matcher "^\\(---\\|\\+\\+\\+\\).*")
				      (0 'diff-file-header))
				     (,(diff-font-lock-make-header-matcher "^@@.*")
				      (0 'diff-header))
				     (,(diff-font-lock-make-header-matcher "^\\+.*")
				      (0 'diff-added))
				     (,(diff-font-lock-make-header-matcher "^\\-.*")
				      (0 'diff-removed))))

(setq message-font-lock-keywords (append message-font-lock-keywords my-diff-font-lock-keywords))

;; fontify cited part of the mail
(defface mail-cited-0-face
  '((t :inherit font-lock-variable-name-face :bold nil :italic t))
  "Face for cited message parts (level 0)."
  :group 'faces)

(defface mail-cited-1-face
  '((t :inherit font-lock-preprocessor-face :bold nil :italic t))
  "Face for cited message parts (level 1)."
  :group 'faces)

(defface mail-cited-2-face
  '((t :inherit font-lock-constant-face :bold nil :italic t))
  "Face for cited message parts (level 2)."
  :group 'faces)

(defface mail-cited-3-face
  '((t :inherit font-lock-function-name-face :bold nil :italic t))
  "Face for cited message parts (level 3)."
  :group 'faces)

(defface mail-cited-4-face
  '((t :inherit font-lock-type-face :bold nil :italic t))
  "Face for cited message parts (level 4)."
  :group 'faces)

(defface mail-cited-5-face
  '((t :inherit font-lock-comment-face :bold nil :italic t))
  "Face for cited message parts (level 5)."
  :group 'faces)

(defface mail-cited-6-face
  '((t :inherit font-lock-comment-delimiter-face :bold nil :italic t))
  "Face for cited message parts (level 6)."
  :group 'faces)

(defvar mail-cited-regexp
  "^\\(\\([[:alpha:]]+\\)\\|\\( *\\)\\)\\(\\(>+ ?\\)+\\)")

(defun mail-fontify-cited ()
  (save-excursion
    (message-goto-body)
    (while (re-search-forward mail-cited-regexp nil t)
      (let* ((str (buffer-substring (line-beginning-position)
				    (point)))
	     (level (mod (string-width (replace-regexp-in-string
					"[^>]" "" str)) 7))
	     (cited-face  (unless (zerop level)
			    (intern-soft (format "mail-cited-%d-face" level)))))
	(when cited-face
	  (add-face-text-property (line-beginning-position)
				  (line-end-position)
				  cited-face))))))

;; send patch
(require 'send-patch)

;; auto detect sender account
(defun mail-get-from-field ()
  (save-excursion
    (message-goto-from)
    (message-beginning-of-line)
    (buffer-substring (point) (line-end-position))))

(defun mail-auto-set-account ()
  (cl-multiple-value-bind (name mail)
      (gnus-extract-address-components (mail-get-from-field))
    (let ((account (seq-find (lambda (account)
			       (string= mail (cadr (cadr account))))
			     mail-accounts-alist)))
      (when account
	(mail-set-vars (car account))))))

(add-hook 'send-patch-compose-hook 'mail-auto-set-account)

;; register kernel backend for send-patch
(defun patch-get-from-field (patch)
  (with-temp-buffer
    (insert-file-contents patch nil 0 256)
    (goto-char (point-min))
    (while (re-search-forward "^From: " nil t))
    (buffer-substring (point) (line-end-position))))

(defun get-address-kernel (patchs)
  (let* ((files-str (mapconcat 'identity patchs " "))
	 (cmd (concat "./scripts/get_maintainer.pl " files-str))
	 (receivers (split-string
		     (shell-command-to-string cmd) "\n"))
	 (user (patch-get-from-field (car patchs)))
	 (cc `("linux-kernel@vger.kernel.org" ,(if user user)))
	 to)
    (mapc (lambda (user)
	    (save-match-data
	      (when (string-match "\\(.*\\) \(\\(.*\\):.*" user)
		(if (string-equal (match-string 2 user)
				  "maintainer")
		    (add-to-list 'to (match-string 1 user))
		  (add-to-list 'cc (match-string 1 user))))))
	  receivers)
    (list to cc)))

(send-patch-register-backend :name "Kernel"
			     :url "git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git"
			     :get-range 'send-patch-get-range-from-last-tag
			     :get-address 'get-address-kernel)

;; register u-boot backend for send-patch
(defun get-address-uboot (patchs)
  (let* ((files-str (mapconcat 'identity patchs " "))
	 (cmd (concat "./scripts/get_maintainer.pl " files-str))
	 (receivers (split-string
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
	  receivers)
    (list to cc)))

(send-patch-register-backend :name "U-Boot"
			     :url "git://git.denx.de/u-boot.git"
			     :get-range 'send-patch-get-range-from-remote-head
			     :get-address 'get-address-uboot)

;; register notmuch backend for send-patch
(defun get-address-notmuch (patchs)
  (list '("notmuch@notmuchmail.org") nil))

(send-patch-register-backend :name "Notmuch"
			     :url "git://git.notmuchmail.org/git/notmuch"
			     :get-range 'send-patch-get-range-from-remote-head
			     :get-address 'get-address-notmuch)

;; default mail client
(require 'my-notmuch)


(provide 'my-mail)
