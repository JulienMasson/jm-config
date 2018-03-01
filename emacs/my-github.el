;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GITHUB CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magit
(require 'magit)
(require 'magit-blame)

;; use emacsclient as the $EDITOR
(require 'with-editor)
(add-hook 'shell-mode-hook 'with-editor-export-editor)

;; ido on magit
(setq magit-completing-read-function 'magit-ido-completing-read)

;; blacklist slow git repository
(setq magit-blacklist-repo '())
(setq magit-status-headers-hook-saved magit-status-headers-hook)
(setq magit-revision-sections-hook-saved magit-revision-sections-hook)

;; revert this change: `magit-visit-ref' behaves just like `magit-show-commit'
(setq magit-visit-ref-behavior '(checkout-any focus-on-ref))

(defun my-magit-status-headers ()
  (let ((magit-insert-headers-hook
	 (if (-contains? magit-blacklist-repo (magit-toplevel))
	     ;; remove tag entry for magit status
	     (remove 'magit-insert-tags-header magit-status-headers-hook-saved)
	   magit-status-headers-hook-saved))
	 wrapper)
    (while (and (setq wrapper (pop magit-insert-headers-hook))
		(= (point) (point-min)))
      (funcall wrapper))))

(defun my-magit-revision-sections (rev)
  (let ((magit-revision-section-hook
	 (if (-contains? magit-blacklist-repo (magit-toplevel))
	     ;; remove revision header in magit-diff
	     (remove 'magit-insert-revision-headers magit-revision-sections-hook-saved)
	   magit-revision-sections-hook-saved)))
    (run-hook-with-args 'magit-revision-section-hook rev)))

(setq magit-status-headers-hook '(my-magit-status-headers))
(setq magit-revision-sections-hook '(my-magit-revision-sections))

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
