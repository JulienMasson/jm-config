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

;; (custom-set-faces
;;  '(magit-section-highlight ((t)))
;;  '(magit-section-heading ((t :inherit font-lock-keyword-face)))
;;  '(magit-diff-file-heading ((t)))
;;  '(magit-diff-added ((t :inherit diff-added)))
;;  '(magit-diff-added-highlight ((t :inherit diff-added)))
;;  '(magit-diff-removed ((t :inherit diff-removed)))
;;  '(magit-diff-removed-highlight ((t :inherit diff-removed)))
;;  '(magit-diff-file-heading-highlight ((t)))
;;  '(magit-diff-hunk-heading-highlight ((t)))
;;  '(magit-diff-context-highlight ((t)))
;;  '(magit-log-date ((t :foreground "DarkSlateGrey"))))

;; shortcut to know if branch is up to date
(setq path-to-check-git-update "~/jm-config/others/check_git_update.sh")
(defun check-git-branch-update (dir-name)
  (interactive "DDirectory: ")
  (shell-command
	   (format "%s %s" path-to-check-git-update (directory-file-name dir-name))))

(provide 'my-github)
