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

;; don't use the system's trash
(setq magit-delete-by-moving-to-trash nil)

;; forge
(require 'forge)

(defun forge-pull-current-topic ()
  (interactive)
  (when-let ((topic (forge-current-topic)))
    (forge-pull-topic topic)))

(defun forge-move-to-diff-post (next-p)
  (let* ((direction (if next-p #'next-line #'previous-line))
	 (limit (if next-p #'eobp #'bobp))
	 (init-post (forge-post-at-point))
	 (post init-post))
    (while (and (not (funcall limit)) (or (not post) (eq init-post post)))
      (funcall direction)
      (setq post (forge-post-at-point)))))

(defun forge-next-diff-post ()
  (interactive)
  (forge-move-to-diff-post t))

(defun forge-previous-diff-post ()
  (interactive)
  (forge-move-to-diff-post nil))

(defun forg-diff-build-versions (versions)
  (let (assoc-versions (count 0))
    (dolist (version versions)
      (cl-incf count)
      (add-to-list 'assoc-versions (cons (number-to-string count) version)))
    assoc-versions))

(defun forge-diff-between-versions ()
  (interactive)
  (if-let* ((pullreq (forge-current-pullreq))
	    (versions (oref pullreq versions))
	    (versions (forg-diff-build-versions versions))
	    (numbers (mapcar #'car versions))
	    (from (completing-read "Diff versions from: " numbers))
	    (from-head (oref (assoc-default from versions) head-ref))
	    (to (completing-read (format "Diff versions from %s to : " from)
				 (remove from numbers)))
	    (to-head (oref (assoc-default to versions) head-ref)))
      (magit-diff-setup-buffer (format "%s..%s" from-head to-head)
			       nil (magit-diff-arguments) nil)
    (error "There is no current pullreq")))

(transient-append-suffix 'magit-diff "w"
  '("v" "Diff between versions" forge-diff-between-versions))

;; show commit
(defun my-magit-show-commit (rev)
  (interactive (list (magit-read-branch-or-commit "Show commit")))
  (pcase-let ((`(,args ,files) (magit-show-commit--arguments)))
    (magit-with-toplevel
      (unless (magit-commit-p rev)
        (user-error "%s is not a commit" rev))
      (magit-revision-setup-buffer rev args files))))

;; display only open topic
(setq forge-topic-list-limit '(60 . 0))

;; don't show magit popup warning
(defun magit--magit-popup-warning ())

;; displays the window at the bottom of the selected frame
(setq transient-display-buffer-action '(display-buffer-below-selected))

;; redefine magit-diff-visit-file
(defun magit-diff-visit-file (file)
  (interactive (list (magit-file-at-point t t)))
  (magit-diff-visit-file--internal file nil #'switch-to-buffer-other-window))

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
  (let ((split-saved (frame-parameter nil 'unsplittable)))
    (when split-saved
      (set-frame-parameter nil 'unsplittable nil))
    (flet ((window-resize (window delta &optional horizontal ignore pixelwise)
			  (my-window-resize window delta horizontal ignore pixelwise)))
      (apply old-fn args))
    (when split-saved
      (set-frame-parameter nil 'unsplittable t))))

(advice-add 'transient--show :around #'transient--show-with-my-window-resize)

;; don’t show any indicators
(setq magit-section-visibility-indicator nil)

;; revert this change: `magit-visit-ref' behaves just like `magit-show-commit'
(setq magit-visit-ref-behavior '(checkout-any focus-on-ref))

;; remove tags section in ref
(delete 'magit-insert-tags magit-refs-sections-hook)

;; blacklist slow git repository
(defvar magit-blacklist-repo '()
  "list of blacklist repository")

(defvar magit-blacklist-status-headers-hook
  (remove 'magit-insert-tags-header magit-status-headers-hook)
  "Remove tags header from status headers")

(defvar magit-blacklist-status-sections-hook
  (seq-filter (lambda (elem)
		(not (member elem '(magit-insert-unpushed-to-pushremote
				    magit-insert-unpulled-from-pushremote
				    magit-insert-unpulled-from-upstream))))
	      magit-status-sections-hook)
  "Remove unpushed/unpulled from status section")

(defun magit-blacklist-filter-hook (hook &rest args)
  (nconc (if (-contains? magit-blacklist-repo (magit-toplevel))
	     (cond ((member 'magit-status-sections-hook hook)
		    (cl-replace hook '(magit-blacklist-status-sections-hook)))
		   ((member 'magit-status-headers-hook hook)
		    (cl-replace hook '(magit-blacklist-status-headers-hook)))
		   (t hook))
	   hook)
	 args))

(advice-add 'magit-run-section-hook :filter-args #'magit-blacklist-filter-hook)

(defun magit-blacklist-insert-revision-headers (old-fn &rest args)
  (if (-contains? magit-blacklist-repo (magit-toplevel))
      (let ((magit-revision-insert-related-refs nil))
  	(apply old-fn args))
    (apply old-fn args)))

(advice-add 'magit-insert-revision-headers :around #'magit-blacklist-insert-revision-headers)

(defun magit-blacklist-branch-or-commit-at-point (old-fn &rest args)
  (if (-contains? magit-blacklist-repo (magit-toplevel))
      (let ((magit-buffer-refname (magit-section-case
  				    (commit (oref it value)))))
  	(apply old-fn args))
    (apply old-fn args)))

(advice-add 'magit-branch-or-commit-at-point :around #'magit-blacklist-branch-or-commit-at-point)

;; set signoff by default
(defvar transient-default-values '((magit-commit "--signoff")))
(setq transient-values transient-default-values)

;; magit log from HEAD to last Tag found
(defun magit-log-from-head-to-last-tag (&optional args files)
  "Show log from `HEAD' to last Tag found."
  (interactive (magit-log-arguments))
  (when-let ((last-tag (magit-git-string "describe" "--abbrev=0" "--tags")))
    (magit-log-setup-buffer (list (format "%s..HEAD" last-tag)) args files)))

(transient-append-suffix 'magit-log "h"
  '("t" "HEAD to last Tag" magit-log-from-head-to-last-tag))

;; magit log from directory
(defun magit-log-directory (dir)
  (interactive "DLog directory: ")
  (let ((default-directory dir))
    (magit-log-setup-buffer (list "HEAD")
                            (car (magit-log-arguments))
                            (list (file-relative-name dir (magit-toplevel))))))

;; magit reset hard HEAD
(defun magit-reset-hard-head ()
  (interactive)
  (magit-reset-hard "HEAD"))

(provide 'my-git)
