;;; my-org.el --- Org Configuration

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

;; org mode and modules
(require 'org)
(require 'org-agenda)
(require 'org-gtasks)
(require 'org-gcal)
(require 'org-tempo)
(require 'ob-gnuplot)
(require 'ob-C)
(require 'ob-shell)
(require 'ob-plantuml)

;; set path to plantuml jar
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; don't archive gcal old events
(setq org-gcal-auto-archive nil)

;; Add a time stamp when a task moves to the DONE state.
(setq org-log-done t)

;; clear files to be used for agenda display
(setq org-agenda-files nil)

;; don't show outline path in echo area after line motion
(setq org-agenda-show-outline-path nil)

;; org todo keywords
(setq org-todo-keywords '((sequence "TODO" "ONGOING" "|" "DONE")))

;; org faces
(setq org-todo-keyword-faces '(("TODO"    . org-todo)
			       ("ONGOING" . font-lock-warning-face)))

;; fold overview
(setq org-startup-folded t)

;; Hook after sorting entries
(add-hook 'org-after-sorting-entries-or-items-hook #'org-set-startup-visibility)

;; show org-mode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)

;; use current window
(setq org-agenda-window-setup 'current-window)

;; default org agenda view
(setq org-agenda-custom-commands '((" " "Agenda" ((agenda "")))))

;; next/previous view
(defun org-agenda-view (direction)
  (org-agenda-check-type t 'agenda)
  (pcase-let ((`(,_ ,days ,span) (get-text-property (point-min) 'org-last-args)))
    (unless days (setq days (org-today)))
    (let* ((cal-iso (calendar-iso-from-absolute days))
	   (cal-gre (calendar-gregorian-from-absolute days))
	   (n (pcase span
		(`day (+ (nth 1 cal-gre) (* direction 1)))
		(`week (+ (car cal-iso) (* direction 1)))
		(`month (+ (car cal-gre) (* direction 1)))
		(`year (+ (nth 2 cal-iso) (* direction 1)))))
	   (sd (org-agenda-compute-starting-span org-starting-day span n))
	   (org-agenda-overriding-cmd (get-text-property (point-min) 'org-series-cmd))
	   (org-agenda-overriding-arguments (list nil sd span)))
      (org-agenda-redo))))

(defun org-agenda-next-view ()
  (interactive)
  (org-agenda-view 1))

(defun org-agenda-previous-view ()
  (interactive)
  (org-agenda-view -1))

;; handle multi accounts gcal
(defvar org-gcal-actions '(("Fetch" . org-gcal-fetch)
			   ("Sync"  . org-gcal-sync)))

(defvar org-gcal-accounts nil)

(defun register-gcal-account (name &rest plist)
  (let ((dir (plist-get plist :directory)))
    (unless (file-directory-p dir)
      (make-directory directory))
    (add-to-list 'org-gcal-accounts (cons name plist))))

(defun org-gcal-set-env (plist)
  (let ((dir (plist-get plist :directory))
	(client-id (plist-get plist :client-id))
	(client-secret (plist-get plist :client-secret))
	(file-alist (plist-get plist :file-alist)))
    (setq org-gcal-dir dir)
    (setq org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-dir))
    (setq org-gcal-client-id client-id)
    (setq org-gcal-client-secret client-secret)
    (setq org-gcal-file-alist file-alist)
    (setq org-gcal-token-plist nil)))

(defun org-gcal ()
  (interactive)
  (when-let* ((collection (mapcar #'car org-gcal-accounts))
	      (name (completing-read "Select Account: " collection))
	      (account (assoc-default name org-gcal-accounts))
	      (action (completing-read (format "Action (%s): " name)
				       (mapcar 'car org-gcal-actions)))
	      (func (assoc-default action org-gcal-actions)))
    (org-gcal-set-env account)
    (funcall func)))

;; disable separator
(setq org-agenda-block-separator nil)

;; my function to display org-agenda
(defun jm-org-agenda ()
  (interactive)
  (org-agenda nil " "))

;; register org entry
(defvar org-tags-todo-list '((agenda "")))
(defun register-org-entry(file &optional tags)
  (unless (file-exists-p file)
    (append-to-file "" nil file))
  (add-to-list 'org-agenda-files file)
  (when tags
    (add-to-list 'org-tags-todo-list `(tags-todo ,tags) t)
    (setq org-agenda-custom-commands `((" " "Agenda" ,org-tags-todo-list)))))

(defun unregister-org-entry ()
  (interactive)
  (let* ((collection (cl-remove-if-not (lambda (elem)
					 (pcase-let ((`(,type . ,value) elem))
					   (eq type 'tags-todo)))
				       org-tags-todo-list))
	 (collection (mapcar #'cadr collection))
	 (target (completing-read "Unregister org entry: " collection)))
    (setq org-tags-todo-list (delete `(tags-todo ,target) org-tags-todo-list))
    (setq org-agenda-custom-commands `((" " "Agenda" ,org-tags-todo-list)))))

;; define own org tags view
(setq org-agenda-prefix-format '((agenda . "%-13i %?-12t% s")
				 (todo . "%-14i")
				 (tags . "%-14i")
				 (search . " %i %-12:c")))

(defun org-agenda-fix-displayed-tags (txt tags add-inherited hide-re)
  (org-trim txt))

(defun org-tags-view (&optional todo-only match)
  (interactive "P")
  (let* ((org--matcher-tags-todo-only todo-only)
	 rtn rtnall matcher)
    (org-agenda-prepare (concat "TAGS " match))
    (setq matcher (org-make-tags-matcher match)
	  match (car matcher)
	  matcher (cdr matcher))
    (org-compile-prefix-format 'tags)
    (org-set-sorting-strategy 'tags)
    (setq org-agenda-query-string match)
    (setq org-agenda-redo-command (list 'org-tags-view
					`(quote ,org--matcher-tags-todo-only)
					`(if current-prefix-arg nil ,org-agenda-query-string)))
    (dolist (file (org-agenda-files nil 'ifmode))
      (org-check-agenda-file file)
      (with-current-buffer (org-get-agenda-file-buffer file)
	(save-excursion
	  (setq rtn (org-scan-tags 'agenda matcher org--matcher-tags-todo-only))
	  (setq rtnall (append rtnall rtn)))))
    (when rtnall
      (insert "\n▶ " (propertize (upcase match) 'face 'org-level-1) "\n")
      (org-agenda-mark-header-line (point-min))
      (insert (org-agenda-finalize-entries rtnall 'tags) "\n")
      (goto-char (point-min))
      (add-text-properties (point-min) (point-max)
      			   `(org-agenda-type tags
      					     org-last-args (,org--matcher-tags-todo-only ,match)
      					     org-redo-cmd ,org-agenda-redo-command
      					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

;; heading done when all checkboxes checked
(require 'org-list)
(defun org-checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((regexp "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]")
      (end (line-end-position))
      next-todo-state)
      (when (re-search-forward regexp end t)
        (if (string= (match-string 1) (match-string 2))
        (setq next-todo-state "DONE")
      (setq next-todo-state "TODO"))
    (unless (string= next-todo-state (org-get-todo-state))
      (org-todo next-todo-state))))))

(add-hook 'org-checkbox-statistics-hook #'org-checkbox-list-complete)

(provide 'my-org)
