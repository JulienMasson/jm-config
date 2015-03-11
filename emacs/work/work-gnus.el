;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GNUS CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offlineimap
(require 'offlineimap)
(run-with-timer 0 (* 1 60) 'offlineimap)

;; gnus-icalendar
(require 'gnus-icalendar)
(setq gnus-icalendar-org-capture-file (expand-file-name "~/org/agenda.org")
      gnus-icalendar-org-capture-headline '("Calendar"))
(gnus-icalendar-setup)
(gnus-icalendar-org-setup)

;; set maildir
(setq gnus-select-method
      '(nnmaildir "Intel"
                  (directory "~/Maildir/Intel")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil)))

;; scan news every minute
(gnus-demon-add-handler 'gnus-demon-scan-news 1 nil) ; this does a call to gnus-group-get-new-news
(gnus-demon-init)

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
  (setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● "
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads-heavy)

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

;; search engine
(require 'notmuch)
(add-hook 'gnus-group-mode-hook 'lld-notmuch-shortcut)
(require 'org-gnus)

(defun lld-notmuch-shortcut ()
  (define-key gnus-group-mode-map "GG" 'notmuch-search)
  )

(defun lld-notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name.
"
  (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
    (setq group (replace-regexp-in-string ".*/Maildir/Intel" "nnmaildir:" group))
    (setq group (replace-regexp-in-string "/$" "" group))
    (if (string-match ":$" group)
        (concat group "INBOX")
      (replace-regexp-in-string ":\\." ":" group))))

(defun lld-notmuch-goto-message-in-gnus ()
  "Open a summary buffer containing the current notmuch
article."
  (interactive)
  (let ((group (lld-notmuch-file-to-group (notmuch-show-get-filename)))
        (message-id (replace-regexp-in-string
                     "^id:" "" (notmuch-show-get-message-id))))
    (setq message-id (replace-regexp-in-string "\"" "" message-id))
    (if (and group message-id)
        (progn
	  (switch-to-buffer "*Group*")
	  (org-gnus-follow-link group message-id))
      (message "Couldn't get relevant infos for switching to Gnus."))))

(define-key notmuch-show-mode-map (kbd "C-c C-c") 'lld-notmuch-goto-message-in-gnus)

;; Ldap tool
;; (require 'ldap-browser)
;; (require 'ldap-browser-mail)
;; (require 'ldap-browser-purple)
;; (defvar ldap-servers '(("ger.corp.intel.com"	.	"ou=Workers,dc=ger,dc=corp,dc=intel,dc=com")
;; 		       ("amr.corp.intel.com"	.	"ou=Workers,dc=amr,dc=corp,dc=intel,dc=com")
;; 		       ("gar.corp.intel.com"	.	"ou=Workers,dc=gar,dc=corp,dc=intel,dc=com")
;; 		       ("ccr.corp.intel.com"	.	"ou=Workers,dc=ccr,dc=corp,dc=intel,dc=com")))

;; addresses completion default ~/.bbdb
(require 'bbdb-loaddefs "/usr/local/share/emacs/site-lisp/bbdb-loaddefs.el")
(add-hook 'message-mode-hook
          '(lambda ()
             (bbdb-initialize 'message)
             (bbdb-initialize 'gnus)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

;; (require 'bbdb)
;; (setq bbdb-file "~/.bbdb")
;; (bbdb-initialize)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

;; (defvar intel-maildir  "--maildir=~/Maildir/Intel")
;; (defvar mu-program     "/usr/local/bin/mu")
;; (defun mu-gnus-update-contacts ()
;;   (interactive)
;;   (async-shell-command
;;    (format "%s cfind --format=bbdb > ~/.bbdb" mu-program)))


(provide 'work-gnus)
