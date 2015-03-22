;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GNUS CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offlineimap
(require 'offlineimap)
(offlineimap)

;; set maildir
(require 'gnus)
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nnmaildir "Gmail"
                  (directory "~/Maildir/Gmail")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil))
	(nnmaildir "Eseo"
                  (directory "~/Maildir/Eseo")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil))
	(nnmaildir "OpenWide"
                  (directory "~/Maildir/OpenWide")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil))))

;; view organisation (put this in .newsrc.eld)
;; (setq gnus-topic-topology '(("Mails" visible) (("Gmail" visible)) (("Eseo" visible)) (("OpenWide" visible))))
;; (setq gnus-topic-alist '(("OpenWide" "nnmaildir+OpenWide:INBOX" "nnmaildir+OpenWide:Sent") ("Eseo" "nnmaildir+Eseo:INBOX" "nnmaildir+Eseo:&AMk-l&AOk-ments envoy&AOk-s") ("Gmail" "nnmaildir+Gmail:INBOX") ("Mails")))

;; rename groups name
(setq gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")
(defun gnus-user-format-function-G (arg)
  (let ((mapped-name (assoc gnus-tmp-group group-name-map)))
    (if (null mapped-name)
        gnus-tmp-group
      (cdr mapped-name))))
(setq group-name-map '(("nnmaildir+Eseo:INBOX" . "INBOX")
		       ("nnmaildir+Eseo:&AMk-l&AOk-ments envoy&AOk-s" . "SENT")
		       ("nnmaildir+Gmail:INBOX" . "INBOX")
		       ("nnmaildir+OpenWide:INBOX" . "INBOX")
		       ("nnmaildir+OpenWide:Sent" . "SENT")))

(setq group-name-map-status '(("nnmaildir+Eseo:INBOX" . "Eseo")
			      ("nnmaildir+Gmail:INBOX" . "Gmail")
			      ("nnmaildir+OpenWide:INBOX" . "OpenWide")))

;; authinfo
(setq smtpmail-auth-credentials "~/.authinfo")

;; w3m render
(setq mm-text-html-renderer 'w3m)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; maximum articles displayed
(setq gnus-newsgroup-maximum-articles 200)

;; scan news every minute
(gnus-demon-add-handler 'gnus-demon-scan-news 1 nil) ; this does a call to gnus-group-get-new-news

;; gnus notifications
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

;; set some gnus features
(setq
 gnus-use-full-window nil
 gnus-summary-line-format "%U%R%z%-16,16&user-date; %*%(%-15,15f%)║ %B %s\n"
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-root "● "
 gnus-sum-thread-tree-false-root " ○ "
 gnus-sum-thread-tree-single-indent " ● "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│"
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

;; eye candy
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(defun oxy-unicode-threads ()
  (interactive)
  (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
	gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "■ "
	gnus-sum-thread-tree-false-root "□ "
	gnus-sum-thread-tree-single-indent "▣ "
	gnus-sum-thread-tree-leaf-with-other "├─▶ "
	gnus-sum-thread-tree-vertical "│"
	gnus-sum-thread-tree-single-leaf "└─▶ "))

(defun oxy-unicode-threads-heavy ()
  (interactive)
  (setq gnus-summary-line-format "%8{%-16,16&user-date;│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "                %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● "
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads-heavy)

;; smtp
(setq user-mail-address "massonju.eseo@gmail.com"
      user-full-name "Julien Masson"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      message-default-headers (concat "Bcc: \"" user-full-name "\" <" user-mail-address ">")
      smtpmail-debug-info nil
      mail-setup-hook nil)

;; addresses completion default ~/.bbdb
(require 'bbdb-loaddefs "~/jm-config/emacs/modules/bbdb/lisp/bbdb-loaddefs.el")
(add-hook 'message-mode-hook
          '(lambda ()
             (bbdb-initialize 'message)
             (bbdb-initialize 'gnus)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

(defvar gmail-maildir  "--maildir=~/Maildir/Gmail")
(defvar eseo-maildir  "--maildir=~/Maildir/Eseo")
(defvar openwide-maildir  "--maildir=~/Maildir/OpenWide")
(defvar mu-program     "/usr/local/bin/mu")

(defun mu-update-database ()
  (shell-command
	   (format "%s index %s" mu-program gmail-maildir))
  (shell-command
	   (format "%s index %s" mu-program eseo-maildir))
  (shell-command
   (format "%s index %s" mu-program openwide-maildir)))

(defun mu-gnus-update-contacts ()
  (interactive)
  (mu-update-database)
  (shell-command
   (format "%s cfind --format=bbdb > ~/.bbdb" mu-program)))

;; search engines
(require 'notmuch)
(add-hook 'gnus-group-mode-hook 'lld-notmuch-shortcut)
(require 'org-gnus)

(defun lld-notmuch-shortcut ()
  (define-key gnus-group-mode-map "GG" 'notmuch-search)
  )

(defun notmuch-update-database ()
  (interactive)
  (async-shell-command "notmuch new"))


(provide 'home-gnus)
