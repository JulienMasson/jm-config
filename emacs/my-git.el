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

;; don't show magit popup warning
(defun magit--magit-popup-warning ())

;; displays the window at the bottom of the selected frame
(setq transient-display-buffer-action '(display-buffer-below-selected))

;; WORKAROUND: transient reduce only the current magit window
(defvar my-window-sibling nil)

(defun my-window-resize (window delta &optional horizontal ignore pixelwise)
  (setq window (window-normalize-window window))
  (let* ((frame (window-frame window))
	 sibling)
    (setq delta (window--size-to-pixel
		 window delta horizontal pixelwise t))
    (cond
     ((or (window--resizable-p
	   window delta horizontal ignore nil nil nil t)
	  (and (not ignore)
	       (setq ignore 'preserved)
	       (window--resizable-p
		window delta horizontal ignore nil nil nil t)))
      (window--resize-reset frame horizontal)
      (window--resize-this-window window delta horizontal ignore t)
      (if (and (not (eq window-combination-resize t))
	       (window-combined-p window horizontal)
               (setq sibling my-window-sibling)
	       (window-sizable-p
		sibling (- delta) horizontal ignore t))
	  ;; If window-combination-resize is nil, WINDOW is part of an
	  ;; iso-combination, and WINDOW's neighboring right or left
	  ;; sibling can be resized as requested, resize that sibling.
	  (let ((normal-delta
		 (/ (float delta)
		    (window-size (window-parent window) horizontal t))))
	    (window--resize-this-window sibling (- delta) horizontal nil t)
	    (set-window-new-normal
	     window (+ (window-normal-size window horizontal)
		       normal-delta))
	    (set-window-new-normal
	     sibling (- (window-normal-size sibling horizontal)
			normal-delta)))
	;; Otherwise, resize all other windows in the same combination.
	(window--resize-siblings window delta horizontal ignore))
      (when (window--resize-apply-p frame horizontal)
	(if (window-resize-apply frame horizontal)
	    (progn
	      (window--pixel-to-total frame horizontal)
	      (run-window-configuration-change-hook frame))
	  (error "Failed to apply resizing %s" window))))
     (t
      (error "Cannot resize window %s" window)))))

(defun transient--show-with-my-window-resize (old-fn &rest args)
  (setq my-window-sibling (selected-window))
  (flet ((window-resize (window delta &optional horizontal ignore pixelwise)
			(my-window-resize window delta horizontal ignore pixelwise)))
    (apply old-fn args)))

(advice-add 'transient--show :around #'transient--show-with-my-window-resize)

;; don’t show any indicators
(setq magit-section-visibility-indicator nil)

;; use emacsclient as the $EDITOR
(require 'with-editor)
(add-hook 'shell-mode-hook 'with-editor-export-editor)

;; ido on magit
(setq magit-completing-read-function 'magit-ido-completing-read)

;; revert this change: `magit-visit-ref' behaves just like `magit-show-commit'
(setq magit-visit-ref-behavior '(checkout-any focus-on-ref))

;; blacklist slow git repository
(defvar magit-blacklist-repo '()
  "list of blacklist repository")

(defvar magit-blacklist-status-headers-hook
  (remove 'magit-insert-tags-header magit-status-headers-hook)
  "Remove tags header from status headers")

(defvar magit-blacklist-status-sections-hook
  (seq-filter (lambda (elem)
		(not (member elem '(magit-insert-unpushed-to-pushremote
				    magit-insert-unpushed-to-upstream-or-recent
				    magit-insert-unpulled-from-pushremote
				    magit-insert-unpulled-from-upstream))))
	      magit-status-sections-hook)
  "Remove unpushed/unpulled from status section")

(defvar magit-blacklist-revision-sections-hook
  (remove 'magit-insert-revision-headers magit-revision-sections-hook)
  "Remove revision header from revision section")

(defun magit-blacklist-filter-hook (hook &rest args)
  (nconc (if (-contains? magit-blacklist-repo (magit-toplevel))
	     (cond ((member 'magit-status-sections-hook hook)
		    (cl-replace hook '(magit-blacklist-status-sections-hook)))
		   ((member 'magit-status-headers-hook hook)
		    (cl-replace hook '(magit-blacklist-status-headers-hook)))
		   ((member 'magit-revision-sections-hook hook)
		    (cl-replace hook '(magit-blacklist-revision-sections-hook)))
		   (t hook))
	   hook)
	 args))

(advice-add 'magit-run-section-hook :filter-args #'magit-blacklist-filter-hook)

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
