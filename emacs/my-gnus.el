;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GNUS CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offlineimap
(require 'offlineimap)

;; gnus basic config
(require 'gnus)

;; method for selecting/reading
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods nil)

;; don't save .newsrc and .newsrc-dribble file
(require 'gnus-start)
(setq gnus-use-dribble-file nil)
(setq gnus-save-newsrc-file nil)
(defun gnus-save-newsrc-file (&optional force))

;; accounts management
(defstruct gnus-account
  name
  folder
  smtp)

(defvar gnus-accounts '())
(defvar default-maildir "~/Maildir/")

(defun register-gnus-nnmaildir-account (name folder smtp)
  (let ((dir (concat default-maildir name))
	(account (make-gnus-account :name name
				    :folder folder
				    :smtp smtp)))
    (add-to-list 'gnus-secondary-select-methods
		 `(nnmaildir ,name
			     (directory ,dir)
			     (directory-files nnheader-directory-files-safe)
			     (get-new-mail nil)))
    (add-to-list 'gnus-accounts account)))

(defun ido-compose-mail (account)
  (interactive (list (ido-completing-read "Account: "
					  (mapcar (lambda (account)
						    (gnus-account-name account))
						    gnus-accounts)
					  nil t nil nil)))
  (let ((func (gnus-account-smtp
	       (find account gnus-accounts :key 'gnus-account-name :test 'string=))))
    (call-if-function func)
    (compose-mail)))

(defun gnus-add-accounts-topic ()
  (mapc (lambda (account)
	   (let ((name (gnus-account-name account))
	   	 (folder (gnus-account-folder account)))
	     (unless (gnus-topic-find-topology name)
	       (gnus-topic-create-topic name "Gnus"))
	     (setq gnus-topic-alist
	   	   (append `(,(append `(,name) folder))
	   		   gnus-topic-alist))))
	 gnus-accounts))

;; topic config
(require 'gnus-topic)
(defun gnus-my-topic ()
  (setq gnus-topic-topology '(("Gnus" visible)))
  (setq gnus-topic-alist '(("Gnus" "nndraft:drafts")))
  (gnus-add-accounts-topic)
  (gnus-group-list-all-groups))

;; (add-hook 'gnus-started-hook 'gnus-my-topic)

;; hook when exiting summary
(add-hook 'gnus-summary-exit-hook 'gnus-group-list-all-groups)

;; rename groups name
(setq gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")
(defun gnus-user-format-function-G (arg)
  (if (string-match ".*:\\(.*\\)" gnus-tmp-group)
      (match-string 1 gnus-tmp-group)
    gnus-tmp-group))

;; gnus render
(setq mm-text-html-renderer 'shr)

;; set verbose gnus message level
(setq gnus-verbose 0)

;; Tree view for groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; gathering loose threads, takes subjects into consideration
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; hide all threads initially
(setq gnus-thread-hide-subtree t)

;; maximum articles displayed
(setq gnus-newsgroup-maximum-articles 200)

;; scan news every minute
(require 'gnus-demon)
(gnus-demon-add-handler 'gnus-demon-scan-news 1 nil)

;; how we display gnus
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

;; bbdb config
(require 'bbdb)
(require 'bbdb-loaddefs)

;; use company on message completion
(require 'company)
(setq message-completion-alist
      (list (cons message-newgroups-header-regexp 'message-expand-group)
	    '("^\\(Resent-\\)?\\(To\\|B?Cc\\):" . company-complete)
	    '("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):"
	      . company-complete)
	    '("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):"
	      . company-complete)))

;; flyspell when composing gnus message
(add-hook 'message-mode-hook 'git-commit-turn-on-flyspell)

;; start functions after load gnus
(add-hook 'gnus-started-hook
	  '(lambda ()
	     (gnus-demon-init)
	     (offlineimap)))

;; start functions after quit gnus
(add-hook 'gnus-after-exiting-gnus-hook
	  '(lambda ()
	     (offlineimap-kill)
	     (offlineimap-quit)))

;; search engine
(require 'nnmairix)
(defun create-search-group-nnmaildir (name)
  (let* ((defaultgroup "Search"))
    ;; Create default search group
    (gnus-group-make-group
     defaultgroup (list 'nnmairix name
			(list 'nnmairix-backend (intern "nnmaildir"))
			(list 'nnmairix-backend-server name)
			(list 'nnmairix-mairix-command "mairix")
			(list 'nnmairix-hidden-folders nil)
			(list 'nnmairix-default-group defaultgroup)))))

;; Use gnus for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'gnus-user-agent))

;; increase lenght of lines
(add-hook 'message-mode-hook
	  (lambda ()
	    (setq fill-column 100)
	    (turn-on-auto-fill)))

;; config regarding sending message
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info nil
      mail-setup-hook nil
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      starttls-extra-arguments nil
      starttls-gnutls-program "/usr/bin/gnutls-cli"
      starttls-extra-arguments nil
      starttls-use-gnutls t)

;; use ido when completing read
(setq gnus-completing-read-function 'gnus-ido-completing-read)

;; attachments
(defun gnus-get-line-article (match)
  (goto-char (point-min))
  (search-forward match nil t)
  (line-number-at-pos))

(defun gnus-build-list-attachments ()
  (with-current-buffer gnus-article-buffer
    (let ((start (gnus-get-line-article "Attachments:\n"))
	  (end (gnus-get-line-article "\n\n"))
	  attachments)
      (goto-line start)
      (while (< (line-number-at-pos) end)
	(right-char 1)
	(let* ((data (get-text-property (point) 'gnus-data))
	       (name (or (mail-content-type-get
			  (mm-handle-disposition data) 'filename)
			 (mail-content-type-get
			  (mm-handle-type data) 'name))))
	  (when (and name data)
	    (add-to-list 'attachments `(,name . ,data))))
	(forward-line 1))
      (delq nil attachments))))

(defun gnus-save-attachment (file handle)
  (when (or (not (file-exists-p file))
	    (yes-or-no-p (format "File %s already exists; overwrite? "
				 file)))
      (mm-save-part-to-file handle file)))

(defun gnus-ido-save-attachments ()
  (interactive)
  (let* ((attachments (gnus-build-list-attachments))
	 (target (ido-completing-read "Save: "
				      (append '("all")
					      (mapcar 'car attachments))
				      nil t nil nil))
	 (dir (ido-read-directory-name "Directory: ")))
    (if (string= target "all")
	(mapc (lambda (data)
		(gnus-save-attachment (expand-file-name (car data) dir)
				      (cdr data)))
	      attachments)
      (let ((file (expand-file-name target dir))
	    (handle (cdr (assoc target attachments))))
	(gnus-save-attachment file handle)))))

;; add personal gmail account
(defvar gnus-perso-folders '("nnmaildir+Gmail:INBOX"
			     "nnmaildir+Gmail:Sent"))
(defun gnus-set-smtp-gmail ()
  (setq user-mail-address "massonju.eseo@gmail.com"
        user-full-name "Masson, Julien"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-message-archive-group "nnmaildir+Gmail:Sent"))

(register-gnus-nnmaildir-account "Gmail" gnus-perso-folders 'gnus-set-smtp-gmail)


(provide 'my-gnus)
