;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GNUS CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gnus
(require 'smtpmail)

(setq 
;; Use Gnus to read gmail from the local directory to which offlineimap syncs 
 gnus-select-method 
 '(nnmaildir "OpenWide" 
	     (directory "~/Maildir/OpenWide/INBOX/") 
	     (expire-age never)) 
 mail-sources '((maildir :path "~/Maildir/OpenWide/INBOX/" :subdirs ("cur" "new"))) 
 mail-source-delete-incoming t

;; (setq ;; SMTP
;;  user-mail-address "massonju.eseo@intel.com"
;;  user-full-name "Julien Masson"
;;  message-send-mail-function 'smtpmail-send-it
;;  smtpmail-smtp-server "smtp.gmail.com"
;;  smtpmail-smtp-service 587
 ;; GNUS
 ;; gnus-select-method '(nnimap "IMAP"
 ;; 			     (nnimap-address "imap.gmail.com")
 ;; 			     (nnimap-server-port 993)
 ;; 			     (nnimap-stream ssl)
 ;; 			     (nnimap-inbox "INBOX"))

 ;; Use Gnus to read gmail from the local directory to which offlineimap syncs
 ;; gnus-select-method '(nnmaildir "Gmail"
 ;; 				(directory "~/Maildir/Gmail/INBOX/")
 ;; 				(expire-age never))
 ;; mail-sources '((maildir :path "~/Maildir/Gmail/INBOX/" :subdirs ("cur" "new")))
 ;; mail-source-delete-incoming t

 gnus-use-scoring t
 gnus-use-adaptive-scoring t
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

(provide 'my-gnus)
