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

;; default message signature
(setq message-signature "Julien Masson")

;; hide In-Reply-To field
(add-to-list 'message-hidden-headers "^In-Reply-To:")

;; message buffer will be killed after sending a message
(setq message-kill-buffer-on-exit t)

;; jmail
(require 'jmail)

;; set top maildir
(setq jmail-top-maildir "~/.cache/mails")

;; set mbsync config file
(setq jmail-sync-config-file (concat my-private-dotfiles-path ".mbsyncrc"))

;; set msmtp config file
(setq jmail-smtp-config-file (concat my-private-dotfiles-path ".msmtprc"))

;; auto-fill queries from top maildir
(setq jmail-queries (jmail-autofill-maildir-queries jmail-top-maildir))

;; add custom queries
(add-to-list 'jmail-queries '(nil . (("Starred"   . "flag:flagged"))))

;; remove drafts, sent and trash from queries
(assoc-delete-all "drafts" jmail-queries)
(assoc-delete-all "sent" jmail-queries)
(assoc-delete-all "trash" jmail-queries)

;; refresh every 60 seconds
(setq jmail-update-buffer-every 60)

;; cached unread data
(defvar jmail-unread-data-cached nil)

(defun jmail--cache-unread-data (query count)
  (if (> count 0)
      (if (assoc query jmail-unread-data-cached)
	  (setcdr (assoc query jmail-unread-data-cached) count)
	(add-to-list 'jmail-unread-data-cached (cons query count)))
    (when (assoc query jmail-unread-data-cached)
      (setq jmail-unread-data-cached
	    (assoc-delete-all query jmail-unread-data-cached #'string=)))))

(add-hook 'jmail-unread-count-hook #'jmail--cache-unread-data)

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

;; send patch
(require 'send-patch)

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


(provide 'my-mail)
