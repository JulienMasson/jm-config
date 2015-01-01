;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GITHUB CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(require 'magit)

;; shortcut to know if branch is up to date
(setq path-to-check-git-update "~/jm-config/others/check_git_update.sh")
(defun check-git-branch-update (dir-name)
  (interactive "DDirectory: ")
  (shell-command
	   (format "%s %s" path-to-check-git-update (directory-file-name dir-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYS SHORTCUTS        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(global-set-key (kbd "C-c g u") 'check-git-branch-update)

(provide 'my-github)
