;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               PIDGIN CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load purple
(load "~/.emacs.d/elpa/purple/purple-group.el")
(load "~/.emacs.d/elpa/purple/purple-buddy.el")
(load "~/.emacs.d/elpa/purple/purple-mail.el")
(load "~/.emacs.d/elpa/purple/purple-chat.el")
(load "~/.emacs.d/elpa/purple/purple-chat-buffer.el")
(load "~/.emacs.d/elpa/purple/purple-account.el")
(load "~/.emacs.d/elpa/purple/purple-status.el")
(load "~/.emacs.d/elpa/purple/purple.el")

;; shortcuts for pidgin using purple
(global-set-key (kbd "C-c p i") 'purple-init)
(global-set-key (kbd "C-c p b") 'purple-buddies-list)
(global-set-key (kbd "C-c p s") 'purple-status-set)
(global-set-key (kbd "C-c p m") 'purple-mail-to)
(global-set-key (kbd "C-c p a") 'purple-buddy-add)
