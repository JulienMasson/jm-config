;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                MAIL CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; mail
(setq user-mail-address "julien.masson@intel.com"
       user-full-name "Masson, Julien"
       smtpmail-smtp-server "mail.intel.com"
       smtpmail-smtp-service 25
       send-mail-function 'smtpmail-send-it
       message-send-mail-function 'smtpmail-send-it
       message-default-headers (concat "Bcc: \"" user-full-name "\"<" user-mail-address ">")
       smtpmail-debug-info nil
       mail-setup-hook nil)

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq
  elmo-maildir-folder-path "~/Maildir"          ;; where i store my mail

  wl-stay-folder-window t                       ;; show the folder pane (left)
  wl-folder-window-width 25                     ;; toggle on/off with 'i'

  ;; imap
  ;;elmo-imap4-default-server "irsmsx151.ger.corp.intel.com"
  elmo-imap4-default-server "imapmail.intel.com"
  elmo-imap4-default-user "julien.masson@intel.com"
  elmo-imap4-default-authenticate-type 'clear
  elmo-imap4-default-port '993
  elmo-imap4-default-stream-type 'ssl
  elmo-imap4-use-modified-utf7 t


;; (setq wl-smtp-connection-type 'starttls)
;; (setq wl-smtp-posting-port 587)
  wl-smtp-posting-server "mail.intel.com"            ;; put the smtp server here
  wl-local-domain "julien.masson@intel.com"          ;; put something here...
  wl-message-id-domain "julien.masson@intel.com"     ;; ...

  wl-from "Julien Masson <julien.masson@intel.com>"                  ;; my From:

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
  ;; the '.'-prefix is for marking them as maildirs
  wl-fcc ".sent"                       ;; sent msgs go to the "sent"-folder
  wl-fcc-force-as-read t               ;; mark sent messages as read
  wl-default-folder ".inbox"           ;; my main inbox
  wl-draft-folder ".drafts"            ;; store drafts in 'postponed'
  wl-trash-folder ".trash"             ;; put trash in 'trash'
  wl-spam-folder ".trash"              ;; ...spam as well
  wl-queue-folder ".queue"             ;; we don't use this

  ;; check this folder periodically, and update modeline
  wl-biff-check-folder-list '(".todo") ;; check every 180 seconds
                                       ;; (default: wl-biff-check-interval)

  ;; hide many fields from message buffers
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '("^From"
    "^Organization:"
    "^X-Attribution:"
     "^Subject"
     "^Date"
     "^To"
     "^Cc"))



;; ;; IMAP
;; (setq elmo-imap4-default-server "imapmail.intel.com")
;; (setq elmo-imap4-default-user "julien.masson@intel.com")
;; (setq elmo-imap4-default-authenticate-type 'clear)
;; (setq elmo-imap4-default-port '993)
;; (setq elmo-imap4-default-stream-type 'ssl)

;; (setq elmo-imap4-use-modified-utf7 t)

;; ;; SMTP
;; (setq wl-smtp-connection-type 'starttls)
;; (setq wl-smtp-posting-port 587)
;; (setq wl-smtp-authenticate-type "plain")
;; (setq wl-smtp-posting-user "Julien Masson")
;; (setq wl-smtp-posting-server "mail.intel.com")
;; (setq wl-local-domain "intel.com")
;; (setq wl-message-id-domain "intel.com")

;; (setq wl-default-folder "%inbox")
;; (setq wl-default-spec "%")
;; (setq wl-draft-folder "%[Intel]/Drafts") ; Intel IMAP
;; (setq wl-trash-folder "%[Intel]/Trash")

;; (setq wl-folder-check-async t)

;; (setq elmo-imap4-use-modified-utf7 t)

;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'wl-user-agent
;;       'wl-user-agent-compose
;;       'wl-draft-send
;;       'wl-draft-kill
;;       'mail-send-hook))

