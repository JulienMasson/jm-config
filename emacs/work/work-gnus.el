;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GNUS CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offlineimap
(require 'offlineimap)

;; set maildir
(require 'gnus)
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nnmaildir "Intel"
                  (directory "~/Maildir/Intel")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil))))

;; view organisation (put this in .newsrc.eld)
;; (setq gnus-topic-topology '(("Mails" visible) (("Intel" visible)) ))
;; (setq gnus-topic-alist '(("Intel" "nnmaildir+Intel:INBOX" "nnmaildir+Intel:Sent" "nnmairix+mysearch:") ("Mails")))

;; rename groups name
(setq gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")
(defun gnus-user-format-function-G (arg)
  (let ((mapped-name (assoc gnus-tmp-group group-name-map)))
    (if (null mapped-name)
        gnus-tmp-group
      (cdr mapped-name))))
(setq group-name-map '(("nnmaildir+Intel:INBOX" . "INBOX")
		       ("nnmairix+mysearch:" . "SEARCH")
		       ("nnmaildir+Intel:Sent" . "SENT")))

(setq group-name-map-status '(("nnmaildir+Intel:INBOX" . "INBOX")
			      ("nnmaildir+Intel:Sent" . "SENT")))

;; authinfo
(setq smtpmail-auth-credentials "~/.authinfo")

;; w3m render
(setq mm-text-html-renderer 'w3m)

;; config w3m
(eval-after-load "w3m"
  '(progn
     (define-key w3m-mode-map [left] 'backward-char)
     (define-key w3m-mode-map [right] 'forward-char)
     (define-key w3m-mode-map [up] 'previous-line)
     (define-key w3m-mode-map [down] 'next-line)
     (define-key w3m-minor-mode-map [left] 'backward-char)
     (define-key w3m-minor-mode-map [right] 'forward-char)
     (define-key w3m-minor-mode-map [up] 'previous-line)
     (define-key w3m-minor-mode-map [down] 'next-line)
     ))

;; set verbose gnus message level
(setq gnus-verbose 0)

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

;; where the mail and news I send
(setq gnus-message-archive-group "nnmaildir+Intel:Sent")

;; scan news every minute
(require 'gnus-demon)
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
  (setq gnus-summary-line-format "%8{%-16,16&user-date;│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "                %8{│%}%(                       %7{║%}%) %6{┏○%}  %S\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● "
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads-heavy)

;; gnus-icalendar
(require 'gnus-icalendar)
(setq gnus-icalendar-org-capture-file (expand-file-name "~/org/agenda.org")
      gnus-icalendar-org-capture-headline '("Calendar"))
(gnus-icalendar-setup)
(gnus-icalendar-org-setup)

;; Scoring
(setq gnus-use-adaptive-scoring t)
(setq gnus-default-adaptive-score-alist
       '((gnus-unread-mark)
         (gnus-ticked-mark (from 4))
         (gnus-dormant-mark (from 5))
         (gnus-del-mark (from -4) (subject -1))
         (gnus-read-mark (from 4) (subject 2))
         (gnus-expirable-mark (from -1) (subject -1))
         (gnus-killed-mark (from -1) (subject -3))
         (gnus-kill-file-mark)
         (gnus-ancient-mark)
         (gnus-low-score-mark)
         (gnus-catchup-mark (from -1) (subject -1))))

;; smtp
(setq user-mail-address "julienx.masson@intel.com"
      user-full-name "Masson, JulienX"
      smtpmail-smtp-server "mail.intel.com"
      smtpmail-smtp-service 25
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      message-default-headers (concat "Bcc: \"" user-full-name "\" <" user-mail-address ">")
      smtpmail-debug-info nil
      mail-setup-hook nil)

;; addresses completion default ~/.bbdb
(require 'bbdb-loaddefs "~/jm-config/emacs/modules/bbdb/lisp/bbdb-loaddefs.el")
(setq bbdb-completion-display-record nil)
(add-hook 'message-mode-hook
          '(lambda ()
             (bbdb-initialize 'message)
             (bbdb-initialize 'gnus)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

;; search engine
(require 'nnmairix)

;; Use gnus for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'gnus-user-agent))

;; eval after load gnus-group
(eval-after-load "gnus-group"
  '(progn
     (gnus-demon-init)))

(provide 'work-gnus)
