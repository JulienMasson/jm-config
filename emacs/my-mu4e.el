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

;; time (secs) to retrieve mail and update the database
(setq mu4e-update-interval 60)

;; write own main view
(defun mu4e~main-view-real (ignore-auto noconfirm)
  (let ((buf (get-buffer-create mu4e~main-buffer-name))
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
       (mu4e-main-mode))))

;; handle multi maildir accounts
(require 'mu4e-maildirs-extension)
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


(provide 'my-mu4e)
