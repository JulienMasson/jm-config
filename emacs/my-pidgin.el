;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               PIDGIN CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load purple
(require 'purple)

;; shortcuts for pidgin using purple
(global-set-key (kbd "C-c p i") 'purple-init)
(global-set-key (kbd "C-c p b") 'purple-buddies-list)
(global-set-key (kbd "C-c p s") 'purple-status-set)
(global-set-key (kbd "C-c p m") 'purple-mail-to)
(global-set-key (kbd "C-c p a") 'purple-buddy-add)

(provide 'my-pidgin)
