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


(provide 'my-mu4e)
