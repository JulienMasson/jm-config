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

;; set Mail User Agent
(setq mail-user-agent 'jmail-user-agent)

;; set top maildir
(setq jmail-top-maildir "~/.cache/mails")

;; set mbsync config file
(setq jmail-sync-config-file (concat my-private-dotfiles-path ".mbsyncrc"))

;; set msmtp config file
(setq jmail-smtp-config-file (concat my-private-dotfiles-path ".msmtprc"))

;; default query options
(setq jmail-default-query-options '(:thread t :auto-fold-thread t :related t))

;; auto-fill queries from top maildir
(apply #'jmail-autofill-queries-from-top-maildir jmail-default-query-options)

;; add custom queries
(jmail-add-query :name "Starred"
		 :query "flag:flagged"
		 :thread t
		 :auto-fold-thread t
		 :related t)

;; RSS news config
(setq jmail-rss-config-file (concat my-private-dotfiles-path ".feed2exec.ini"))

;; fetch/refresh every 3 mins
(setq jmail-fetch-refresh-every (* 3 60))

;; fetch/refresh RSS news every 30 mins
(setq jmail-rss-fetch-refresh-every (* 30 60))

;; display html by default
(setq jmail-view-html-default-view t)

;; enable org-msg for jmail
(jmail-org-msg-enable)

;; org msg
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t")
(setq org-msg-startup "hidestars noindent inlineimages")
(setq org-msg-default-alternatives '(text html))
(setq org-msg-convert-citation t)
(setq org-msg-signature "
#+begin_signature

Julien Masson
#+end_signature")

;; cached unread data
(defvar jmail-unread-data-cached nil)

(defun jmail--cache-unread-data (props count)
  (when-let ((query (plist-get props :query)))
    (if (> count 0)
	(if (assoc query jmail-unread-data-cached)
	    (setcdr (assoc query jmail-unread-data-cached) count)
	  (add-to-list 'jmail-unread-data-cached (cons query count)))
      (when (assoc query jmail-unread-data-cached)
	(setq jmail-unread-data-cached
	      (assoc-delete-all query jmail-unread-data-cached #'string=))))))

(add-hook 'jmail-unread-count-hook #'jmail--cache-unread-data)

(defun jmail--clear-cache-unread ()
  (setq jmail-unread-data-cached nil))

(add-hook 'jmail-quit-hook #'jmail--clear-cache-unread)

;; send patch
(require 'send-patch)

;; setup jmail env when sending patchs
(defun send-patch-setup-jmail-env ()
  (when-let ((accounts (jmail-get-accounts jmail-smtp-config-file))
	     (from (message-fetch-field "From")))
    (cl-multiple-value-bind (name email)
	(gnus-extract-address-components from)
      (when-let ((account (seq-find (lambda (elem)
				      (string= email (cddr elem)))
				    accounts)))
	(jmail-compose-mode)
	(jmail-company-setup)
	(jmail-compose-setup-send-mail)
	(jmail-compose-set-extra-arguments (car account) email)))))

(add-hook 'send-patch-compose-hook 'send-patch-setup-jmail-env)

;; add kernel recipients function
(defun patch-get-from-field (patch)
  (with-temp-buffer
    (insert-file-contents patch nil 0 256)
    (goto-char (point-min))
    (while (re-search-forward "^From: " nil t))
    (buffer-substring (point) (line-end-position))))

(defun send-patch-get-recipients-kernel (patchs)
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

(add-to-list 'send-patch-get-recipients-funcs
	     (cons "Kernel" 'send-patch-get-recipients-kernel) t)

;; add uboot recipients function
(defun send-patch-get-recipients-uboot (patchs)
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

(add-to-list 'send-patch-get-recipients-funcs
	     (cons "U-Boot" 'send-patch-get-recipients-uboot) t)

(provide 'my-mail)
