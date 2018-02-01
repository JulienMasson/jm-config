;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               MU4E CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load mu4e module
(require 'mu4e)

;; maildir root path
(setq mu4e-maildir "/home/lab/Maildir")

;; command to fetch mail
(setq mu4e-get-mail-command "offlineimap")

;; hide index messages
(setq mu4e-hide-index-messages t)

;; fancy chars
(setq mu4e-use-fancy-chars t)

;; time (secs) to retrieve mail and update the database
(setq mu4e-update-interval 60)

;; write own main view
(defvar my-mu4e~main-buffer-name "*mail*")
(defun mu4e~main-view ()
  (let ((buf (get-buffer-create my-mu4e~main-buffer-name))
	(inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       "\n"
       (propertize "  Help Commands\n\n" 'face 'mu4e-title-face)
       (mu4e~main-action-str "\t* [j]ump to some maildir\n" 'mu4e-jump-to-maildir)
       (mu4e~main-action-str "\t* [s]earch query\n" 'mu4e-search)
       (mu4e~main-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new)
       (mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit))
      (mu4e-main-mode)
      (goto-char (point-min)))
    (add-to-list 'global-mode-string '(:eval (mu4e-context-label)))))

;; change headers
(setq mu4e-headers-date-format "%R  %d %b %G  %A")
(setq mu4e-headers-fields '((:date          .  32)
			    (:from          .  25)
			    (:subject       .  nil)))
(setq mu4e-view-fields '(:from :to :cc :subject :date :mailing-list :attachments :signature))

;; enable inline images
(setq mu4e-view-show-images t)

;; body display on the html-version
(setq mu4e-view-prefer-html t)

;; show full addresses in view message
(setq mu4e-view-show-addresses 't)

;; message buffer will be killed after sending a message
(setq message-kill-buffer-on-exit t)

;; handle multi maildir accounts
(require 'mu4e-maildirs-extension)
(setq mu4e-maildirs-extension-buffer-name my-mu4e~main-buffer-name
      mu4e-maildirs-extension-maildir-indent 0
      mu4e-maildirs-extension-maildir-default-prefix " "
      mu4e-maildirs-extension-maildir-collapsed-prefix " "
      mu4e-maildirs-extension-maildir-expanded-prefix "*"
      mu4e-maildirs-extension-updating-string ""
      mu4e-maildirs-extension-maildir-format "\t%i%p %-20n (%u/%t)"
      mu4e-maildirs-extension-maildir-hl-regex mu4e-maildirs-extension-maildir-format)
(mu4e-maildirs-extension)

;; insert maildir header above "Help Commands" header
(setq mu4e-maildirs-extension-insert-before-str "\n  Help Commands")

;; don't display action on maildir header
(setq mu4e-maildirs-extension-action-text nil)

;; change title
(setq mu4e-maildirs-extension-title "  Accounts\n")

;; don't display some folders
(setq mu4e-maildirs-extension-ignored-regex "\\(/drafts\\|/sent\\|trash\\)")

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

;; account list
(defvar my-mu4e-account-alist
  '(("Gmail"
     (user-mail-address "massonju.eseo@gmail.com")
     (user-full-name "Masson, Julien")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; ido attachment
(defstruct mu4e-attachments
  name
  index
  docid)

(defun mu4e-build-attachments-list ()
  (let* ((msg (mu4e-message-at-point))
	 (docid (mu4e-message-field msg :docid)))
    (mapcar (lambda (count)
	      (let* ((att (mu4e~view-get-attach msg count))
		     (name (plist-get att :name))
		     (index (plist-get att :index)))
		(make-mu4e-attachments :name name
				       :index index
				       :docid docid)))
	    (number-sequence 1
			     (hash-table-count mu4e~view-attach-map)))))

(defun mu4e-save-attachment (file data)
  (let ((docid (mu4e-attachments-docid data))
	(index (mu4e-attachments-index data)))
    (mu4e~proc-extract 'save docid index mu4e-decryption-policy file)))

(defun mu4e-ido-save-attachments ()
  (interactive)
  (let* ((attachments (mu4e-build-attachments-list))
	 (target (ido-completing-read "Save: "
				      (append '("all")
					      (mapcar (lambda (data)
							(mu4e-attachments-name data))
						      attachments))
				      nil t nil nil))
	 (dir (ido-read-directory-name "Directory: ")))
    (if (string= target "all")
	(mapc (lambda (data)
		(let ((target (mu4e-attachments-name data)))
		  (mu4e-save-attachment (expand-file-name target dir)
					data)))
	      attachments)
      (let ((file (expand-file-name target dir))
	    (data (find target attachments :key 'mu4e-attachments-name :test 'string=)))
	(mu4e-save-attachment file data)))))


(provide 'my-mu4e)
