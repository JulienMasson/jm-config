;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            WANDERLUST CONFIG              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offlineimap
(require 'offlineimap)
(run-with-timer 0 (* 1 60) 'offlineimap)

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

;; wanderlust
(require 'wl)
(autoload 'wl "wl" "Wanderlust" t)
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

  ;; check interval every 30 seconds
  wl-biff-check-interval 30

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

;; Use wanderlust for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; search using mutt
(defvar intel-maildir  "--maildir=~/Maildir/Intel")
(defvar mu-wl-mu-program     "/usr/local/bin/mu")
(defvar mu-wl-search-folder  "search")
(defun mu-wl-search ()
  "search for messages with `mu', and jump to the results"
   (let* ((muexpr (read-string "Find messages matching: "))
          (sfldr  (concat elmo-maildir-folder-path "/"
                    mu-wl-search-folder))
          (cmdline (concat mu-wl-mu-program " find "
                      "--clearlinks --format=links --linksdir='" sfldr "' "
                     muexpr))
          (rv (shell-command cmdline)))
    (cond
      ((= rv 0)  (message "Query succeeded"))
      ((= rv 2)  (message "No matches found"))
      (t (message "Error running query")))
  (= rv 0)))

(defun mu-wl-search-and-goto ()
  "search and jump to the folder with the results"
  (interactive)
  (when (mu-wl-search)
    (wl-summary-goto-folder-subr
      (concat "." mu-wl-search-folder)
      'force-update nil nil t)
    (wl-summary-sort-by-date)))

;; search by pressing 'Q'
(eval-after-load "wl"
    '(progn
       (define-key wl-summary-mode-map (kbd "Q") ;; => query
	 '(lambda()(interactive)(mu-wl-search-and-goto)))
       (define-key wl-folder-mode-map (kbd "Q") ;; => query
	 '(lambda()(interactive)(mu-wl-search-and-goto)))
       )
)

(defun mu-wl-update-database ()
  (interactive)
  (shell-command
	   (format "%s index %s" mu-wl-mu-program intel-maildir)))
(defun mu-wl-update-contacts ()
  (interactive)
  (shell-command
	   (format "%s cfind --format=wl > ~/.addresses" mu-wl-mu-program)))


(provide 'work-wl)
