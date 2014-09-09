;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                MAIL CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wanderlust
(setq
  elmo-maildir-folder-path "~/Maildir/Intel"          ;; where i store my mail

  wl-stay-folder-window t                       ;; show the folder pane (left)
  wl-folder-window-width 25                     ;; toggle on/off with 'i'

  wl-smtp-posting-server "smtp.intel.com"            ;; put the smtp server here
  wl-local-domain "julienx.masson@intel.com"          ;; put something here...
  wl-message-id-domain "julienx.masson@intel.com"     ;; ...

  wl-from "Julien Masson <julienx.masson@intel.com>"                  ;; my From:

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
  ;; the '.'-prefix is for marking them as maildirs
  wl-fcc ".Sent"                       ;; sent msgs go to the "sent"-folder
  wl-fcc-force-as-read t               ;; mark sent messages as read
  wl-default-folder ".INBOX"           ;; my main inbox
  wl-draft-folder ".Drafts"            ;; store drafts in 'postponed'
  wl-trash-folder ".Trash"             ;; put trash in 'trash'

  ;; check this folder periodically, and update modeline
  wl-biff-check-folder-list '(".INBOX") ;; check every 180 seconds
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
