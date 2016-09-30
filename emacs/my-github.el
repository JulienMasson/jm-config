;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GITHUB CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(require 'magit)
(require 'magit-blame)

;; remove tag entry for magit status
(setq magit-status-headers-hook (remove 'magit-insert-tags-header magit-status-headers-hook))

;; turn on flyspell on git commit
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

;; set signoff by default
(setq magit-commit-arguments (quote ("--signoff")))

; redefine key in magit mode
(define-key magit-mode-map [remap magit-copy-buffer-thing-as-kill] 'kill-ring-save)
(define-key magit-mode-map [remap magit-copy-buffer-revision] 'kill-ring-save)

;; magit push gerrit
(defun magit-git-push-gerrit (branch target args)
  (run-hooks 'magit-credential-hook)
  (-let [(remote . target)
         (magit-split-branch-name target)]
    (magit-run-git-async "push" "-v" args remote
                         (format "%s:refs/for/%s" branch target))))

(defun magit-push-gerrit (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (magit-read-local-branch-or-commit "Push")))
     (list source
           (magit-read-remote-branch (format "Push %s to" source) nil
                                     (magit-get-upstream-branch source)
                                     source 'confirm)
           (magit-push-arguments))))
  (magit-git-push-gerrit source target args))

(magit-define-popup-action 'magit-push-popup
  ?g "Gerrit" 'magit-push-gerrit)


(provide 'my-github)
