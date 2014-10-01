;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GITHUB CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(require 'magit)

;; shortcuts for magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)
(global-set-key (kbd "C-c g f") 'magit-file-log)
(global-set-key (kbd "C-c g p") 'magit-pull)
(global-set-key (kbd "C-c g r") 'magit-reset-head-hard)
(global-set-key (kbd "C-c g a") 'magit-commit-amend)
(global-set-key (kbd "C-c g b") 'magit-blame-mode)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g n") 'magit-create-branch)
(global-set-key (kbd "C-c g d") 'magit-delete-branch)
(global-set-key (kbd "C-c g m") 'magit-branch-manager)
