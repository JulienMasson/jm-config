;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GITHUB CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(require 'magit)
(require 'magit-blame)

;; shortcut to know if branch is up to date
(setq path-to-check-git-update "~/jm-config/others/check_git_update.sh")
(defun check-git-branch-update (dir-name)
  (interactive "DDirectory: ")
  (shell-command
	   (format "%s %s" path-to-check-git-update (directory-file-name dir-name))))


(provide 'my-github)
