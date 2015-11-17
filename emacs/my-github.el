;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GITHUB CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(require 'magit)
(require 'magit-blame)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-git-executable "~/bin/git-install/bin/git")
(setq vc-handled-backends nil)
;; (setq magit-revert-buffers nil)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(define-key magit-mode-map [remap magit-copy-buffer-thing-as-kill] 'kill-ring-save)
(define-key magit-mode-map [remap magit-copy-buffer-revision] 'kill-ring-save)

;; shortcut to know if branch is up to date
(setq path-to-check-git-update "~/jm-config/others/check_git_update.sh")
(defun check-git-branch-update (dir-name)
  (interactive "DDirectory: ")
  (shell-command
	   (format "%s %s" path-to-check-git-update (directory-file-name dir-name))))

(provide 'my-github)
