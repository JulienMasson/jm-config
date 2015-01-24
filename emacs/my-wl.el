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
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(setq wl-insert-message-id nil)
(setq wl-summary-width 255)
(setq wl-stay-folder-window t)                  ;; show the folder pane (left)
(setq wl-folder-window-width 35)                     ;; toggle on/off with 'i'
(setq wl-summary-search-parent-by-subject-regexp nil)

;;Pour les accents
(setq-default mime-transfer-level 8)

;;on affiche d'abord le plain text, sinon l'html
(setq mime-view-type-subtype-score-alist
	  '(((text . plain) . 3)
 		((text . enriched) . 2)
 		((text . richtext) . 1)
		((text . html) . 0)
		(t . 0))
)

;; where i store my mail
(setq elmo-maildir-folder-path "~/Maildir")

;;smtp
(require 'starttls)
(setq starttls-extra-arguments '("--insecure"))
(setq starttls-program '("gnutls-bin"))

;;répertoires
(setq wl-draft-folder "+~/Maildir/draft")
(setq wl-queue-folder "+~/Maildir/queue")
(setq wl-trash-folder "+~/Maildir/trash")

;;modes
(add-hook 'wl-draft-mode-hook 'turn-on-auto-fill) ;;auto fill quand on écrit un mail

;;auto check
 (setq wl-folder-check-async t)
 (setq wl-biff-check-folder-list '(".~/Maildir/Gmail/INBOX"
 				  ".~/Maildir/Eseo/INBOX"
 				  ".~/Maildir/OpenWide/INBOX"))
 (setq wl-biff-check-interval 120) ;;2 minutes

;;notifications et interrogation
;; (add-hook 'wl-biff-notify-hook
;; 		  (lambda()
;; 			(chep-notify-desktop "Wanderlust" "Nouveau mail!!!" (expand-file-name "~/.emacs.d/icons/mail.png" 20))))

;; Set mail-icon to be shown universally in the modeline.
;; (setq global-mode-string
;;       (cons
;;        '(wl-modeline-biff-status
;;          wl-modeline-biff-state-on
;;          wl-modeline-biff-state-off)
;;        global-mode-string))

;;choose template with C-c C-j
(setq wl-template-alist
      '(("Gmail"
         (wl-from . "Julien Masson <massonju.eseo@gmail.com>")
         ("From" . wl-from)
		 (wl-smtp-connection-type . 'starttls)
		 (wl-smtp-posting-port . 587)
		 (wl-smtp-authenticate-type . "clear")
		 (wl-smtp-posting-user . "massonju.eseo@gmail.com")
		 (wl-smtp-posting-server . "smtp.gmail.com")
		 (wl-local-domain . "gmail.com"))
	("Eseo"
         (wl-from . "Julien Masson <julien.masson@reseau.eseo.fr>")
         ("From" . wl-from)
		 (wl-smtp-connection-type . 'starttls)
		 (wl-smtp-posting-port . 587)
		 (wl-smtp-authenticate-type . "clear")
		 (wl-smtp-posting-user . "julien.masson@reseau.eseo.fr")
		 (wl-smtp-posting-server . "smtp.office365.com")
		 (wl-local-domain . "office365.com"))
	("OpenWide"
	     (wl-from . "Julien Masson <julien.masson@openwide.fr>")
         ("From" . wl-from)
		 (wl-fcc . ".~/Maildir/OpenWide/Sent")
		 ("Fcc" . wl-fcc)
		 (wl-smtp-connection-type . 'starttls)
		 (wl-smtp-posting-port . 587)
		 (wl-smtp-authenticate-type . "clear")
		 (wl-smtp-posting-user . "julien.masson@openwide.fr")
		 (wl-smtp-posting-server . "zimbra3.corp.accelance.fr")
		 (wl-local-domain . "accelance.fr"))
))

;; ;; envelope
;; (setq wl-user-mail-address-list '("massonju.eseo@gmail.com" "julien.masson@reseau.eseo.fr" "julien.masson@openwide.fr"))

;;select correct email address when we _start_ writing a draft.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
(setq wl-draft-config-alist
      '(((string-match ".~/Maildir/Gmail" wl-draft-parent-folder)
		 (template . "Gmail")
         (wl-local-domain . "googlemail.com"))
		((string-match ".~/Maildir/Eseo" wl-draft-parent-folder)
		 (template . "Eseo")
         (wl-local-domain . "office365.com"))
		((string-match ".~/Maildir/OpenWide" wl-draft-parent-folder)
		 (template . "OpenWide")
         (wl-local-domain . "openwide.fr"))
))

;; ;; Use different signature files based on From: address
;; ;; (setq signature-file-alist
;; ;;       `((("From" . "cedric.chepied@openwide.fr") . ,(expand-file-name "~/.emacs.d/signature.d/cedric.chepied@openwide.fr"))
;; ;; 		(("From" . "ptit.chep@gmail.com") . ,(expand-file-name "~/.emacs.d/signature.d/ptit.chep@gmail.com"))
;; ;; 		(("From" . "cedric.chepied@gmail.com") . ,(expand-file-name "~/.emacs.d/signature.d/cedric.chepied@gmail.com"))))

;;default folder name auto completion:
(setq wl-default-spec "%")

;; mark sent messages (folder carbon copy) as read.
(setq wl-fcc-force-as-read    t)

;; ;;Only save draft when I tell it to! (C-x C-s or C-c C-s):
;; ;;(arg: seconds of idle time untill auto-save).
;; (setq wl-auto-save-drafts-interval nil)


;;hide many fields from message buffers
(setq wl-message-ignored-field-list '("^.*:"))
(setq wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"
    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    "^Organization:"
    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
	"^X-Mailer"
	"^User-Agent"
    ))

(setq wl-message-sort-field-list
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


;; ;;filtres
;; ;; (setq wl-refile-rule-alist '( ("From"
;; ;; 							   (".*kytos@hotmail.fr.*" . "De Ninie")
;; ;; 							   (".*@raspberrypi.org.*" . "Raspberry pi")
;; ;; 							   (".*gestion31@squarehabitat.fr" . "Appartement"))
;; ;; 							  (("To" "Cc")
;; ;;                                (".*cheppoubelle@hotmail.fr" . "ChepPoubelle"))))

;; ;;pièces jointes
;; (setq mime-edit-split-message nil)


;; ;;Répondre à une seule personne
;; (setq wl-draft-reply-without-argument-list
;;   '(("Reply-To" ("Reply-To") nil nil)
;;      ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
;;      ("From" ("From") nil nil)))

;; ;; Répondre à tous
;; (setq wl-draft-reply-with-argument-list
;;   '(("Followup-To" nil nil ("Followup-To"))
;;      ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
;;      ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
;;      ("From" ("From") ("To" "Cc") ("Newsgroups"))))


;; programme externe
;; ( unless ( functionp #'mime-preview-play-current-entity-orig )
;;   ( fset #'mime-preview-play-current-entity-orig
;; 		 ( symbol-function #'mime-preview-play-current-entity )))

;; ( setq mime-play-delete-file-immediately nil)

;; ( defun mime-preview-play-current-entity ( &optional ignore-examples mode )
;;   ( interactive "P" )
;;   ( if ( and mode ( not ( equal mode "play" )))
;; 	  ( mime-preview-play-current-entity-orig ignore-examples mode )
;; 	( let* (( entity ( get-text-property ( point ) ' mime-view-entity ))
;;             ( name ( mime-entity-safe-filename entity ))
;;             ( filename ( expand-file-name ( if ( and name ( not ( string= name "" )))
;; 											  name
;; 											( make-temp-name "EMI" ))
;; 										  ( make-temp-file "EMI" ' directory ))))
;; 	  ( mime-write-entity-content entity filename )
;; 	  (setq my-mime-preview-play-current-entity-appname
;; 			(completing-read "Programme à lancer: "
;; 							 chep-wl-external-methods
;; 							 nil nil))
;; 	  ( message "External method is starting..." )
;; 	  ( let* (( process-name
;; 				( concat my-mime-preview-play-current-entity-appname " " filename ))
;;               ( process
;; 				( start-process process-name
;; 								mime-echo-buffer-name
;; 								my-mime-preview-play-current-entity-appname
;; 								filename )))
;; 		( set-alist ' mime-mailcap-method-filename-alist process filename )
;; 		( set-process-sentinel process ' mime-mailcap-method-sentinel )))))


;; Les colonnes du summary
;; (setq wl-summary-line-format "%T%P %W %D/%M/%Y %h:%m %t%[%17(%c %f%) %] %s")
;; (setq wl-summary-showto-folder-regexp ".*Messages envoy&AOk-s.*\\|.*Sent.*")

;; ;;ldap
;; (setq wl-use-ldap t
;;       wl-ldap-server "zimbra2.corp.accelance.fr"
;;       wl-ldap-port 389
;; )








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                CONFIG INTEL                          ::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wanderlust
;; (setq
;;   elmo-maildir-folder-path "~/Maildir/Intel"          ;; where i store my mail

;;   wl-stay-folder-window t                       ;; show the folder pane (left)
;;   wl-folder-window-width 25                     ;; toggle on/off with 'i'

;;   wl-smtp-posting-server "smtp.intel.com"            ;; put the smtp server here
;;   wl-local-domain "julienx.masson@intel.com"          ;; put something here...
;;   wl-message-id-domain "julienx.masson@intel.com"     ;; ...

;;   wl-from "Julien Masson <julienx.masson@intel.com>"                  ;; my From:

;;   ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
;;   ;; the '.'-prefix is for marking them as maildirs
;;   wl-fcc ".Sent"                       ;; sent msgs go to the "sent"-folder
;;   wl-fcc-force-as-read t               ;; mark sent messages as read
;;   wl-default-folder ".INBOX"           ;; my main inbox
;;   wl-draft-folder ".Drafts"            ;; store drafts in 'postponed'
;;   wl-trash-folder ".Trash"             ;; put trash in 'trash'

;;   ;; check this folder periodically, and update modeline
;;   wl-biff-check-folder-list '(".INBOX") ;; check every 180 seconds
;;                                        ;; (default: wl-biff-check-interval)

;;   ;; hide many fields from message buffers
;;   wl-message-ignored-field-list '("^.*:")
;;   wl-message-visible-field-list
;;   '("^\\(To\\|Cc\\):"
;;     "^Subject:"
;;     "^\\(From\\|Reply-To\\):"
;;     "^Organization:"
;;     "^Message-Id:"
;;     "^\\(Posted\\|Date\\):"
;;     )
;;   wl-message-sort-field-list
;;   '("^From"
;;     "^Organization:"
;;     "^X-Attribution:"
;;      "^Subject"
;;      "^Date"
;;      "^To"
;;      "^Cc"))

(provide 'my-wl)
