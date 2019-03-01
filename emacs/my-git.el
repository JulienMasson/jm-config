;;; my-git.el --- Git Configuration

;; Copyright (C) 2019 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; URL: https://github.com/JulienMasson/jm-config/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; magit
(require 'magit)
(require 'magit-blame)

;; forge
(require 'forge)

;; don’t show any indicators
(setq magit-section-visibility-indicator nil)

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
  (let ((magit-revision-sections-hook
	 (if (-contains? magit-blacklist-repo (magit-toplevel))
	     ;; remove revision header in magit-diff
	     (remove 'magit-insert-revision-headers magit-revision-sections-hook-saved)
	   magit-revision-sections-hook-saved)))
    (run-hook-with-args 'magit-revision-sections-hook rev)))

(setq magit-status-headers-hook '(my-magit-status-headers))
(setq magit-revision-sections-hook '(my-magit-revision-sections))

;; set signoff by default
(setq magit-commit-arguments (quote ("--signoff")))

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

;; magit log from HEAD to last Tag found
(defun magit-log-from-head-to-last-tag (&optional args files)
  "Show log from `HEAD' to last Tag found."
  (interactive (magit-log-arguments))
  (let ((last-tag (magit-git-string "describe" "--abbrev=0" "--tags")))
    (when last-tag
      (magit-log (list (format "%s..HEAD" last-tag)) args files))))

(magit-define-popup-action 'magit-log-popup
  ?t "Log from HEAD to last Tag" 'magit-log-from-head-to-last-tag)


(provide 'my-git)
