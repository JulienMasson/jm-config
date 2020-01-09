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

;; org todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "UNMERGED" "|" "DONE")))

;; org faces
(setq org-todo-keyword-faces '(("TODO" . org-todo)
			       ("WORKING" . font-lock-warning-face)
			       ("UNMERGED" . font-lock-type-face)))

;; Hook after sorting entries
(add-hook 'org-after-sorting-entries-or-items-hook #'org-set-startup-visibility)

;; show org-mode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; use current window
(setq org-agenda-window-setup 'current-window)

;; default org agenda view
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "")))))

;; my function to display org-agenda
(defun jm-org-agenda ()
  (interactive)
  (org-agenda nil " "))

;; run custom hook when redo all org agenda
(defvar org-agenda-redo-all-hook nil)
(defun org-agenda-redo-all-run-hook (&optional exhaustive)
  (run-hooks 'org-agenda-redo-all-hook))
(advice-add 'org-agenda-redo-all :before #'org-agenda-redo-all-run-hook)

;; register org entry
(defvar org-tags-todo-list '((agenda "")))
(defun register-org-entry(file &optional sync tags)
  (unless (file-exists-p file)
    (append-to-file "" nil file))
  (setq org-agenda-files (append org-agenda-files `(,file)))
  (if (functionp sync)
      (add-hook 'org-agenda-redo-all-hook (eval #'sync)))
  (when tags
    (add-to-list 'org-tags-todo-list `(tags-todo ,tags) t)
    (setq org-agenda-custom-commands
	  `((" " "Agenda"
	     ,org-tags-todo-list)))))

;; define own org tags view
(setq org-agenda-prefix-format '((agenda . "%-13i %?-12t% s")
				 (todo . "%-14i")
				 (tags . "%-14i")
				 (search . " %i %-12:c")))

(defun org-agenda-format-item (extra txt &optional level category tags dotime
				     remove-re habitp)
  (let* ((bindings (car org-prefix-format-compiled))
	 (formatter (cadr org-prefix-format-compiled)))
    (cl-loop for (var value) in bindings
	     do (set var value))
    (save-match-data
      ;; Diary entries sometimes have extra whitespace at the beginning
      (setq txt (org-trim txt))

      (let* ((category (or category
			   (if buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name))
			     "")))
	     (category-icon (org-agenda-get-category-icon category))
	     (category-icon (if category-icon
				(propertize " " 'display category-icon)
			      ""))
	     (effort (and (not (string= txt ""))
			  (get-text-property 1 'effort txt)))
	     ;; time, tag, effort are needed for the eval of the prefix format
	     (tag (if tags (nth (1- (length tags)) tags) ""))
	     (time-grid-trailing-characters (nth 2 org-agenda-time-grid))
	     time
	     (ts (if dotime (concat
			     (if (stringp dotime) dotime "")
			     (and org-agenda-search-headline-for-time txt))))
	     (time-of-day (and dotime (org-get-time-of-day ts)))
	     stamp plain s0 s1 s2 rtn srp l
	     duration breadcrumbs)
	(and (derived-mode-p 'org-mode) buffer-file-name
	     (add-to-list 'org-agenda-contributing-files buffer-file-name))
	(when (and dotime time-of-day)
	  ;; Extract starting and ending time and move them to prefix
	  (when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		    (setq plain (string-match org-plain-time-of-day-regexp ts)))
	    (setq s0 (match-string 0 ts)
		  srp (and stamp (match-end 3))
		  s1 (match-string (if plain 1 2) ts)
		  s2 (match-string (if plain 8 (if srp 4 6)) ts))

	    ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	    ;; them, we might want to remove them there to avoid duplication.
	    ;; The user can turn this off with a variable.
	    (if (and org-prefix-has-time
		     org-agenda-remove-times-when-in-prefix (or stamp plain)
		     (string-match (concat (regexp-quote s0) " *") txt)
		     (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		     (if (eq org-agenda-remove-times-when-in-prefix 'beg)
			 (= (match-beginning 0) 0)
		       t))
		(setq txt (replace-match "" nil nil txt))))
	  ;; Normalize the time(s) to 24 hour
	  (if s1 (setq s1 (org-get-time-of-day s1 'string t)))
	  (if s2 (setq s2 (org-get-time-of-day s2 'string t)))

	  ;; Try to set s2 if s1 and
	  ;; `org-agenda-default-appointment-duration' are set
	  (when (and s1 (not s2) org-agenda-default-appointment-duration)
	    (setq s2
		  (org-duration-from-minutes
		   (+ (org-duration-to-minutes s1 t)
		      org-agenda-default-appointment-duration)
		   nil t)))

	  ;; Compute the duration
	  (when s2
	    (setq duration (- (org-duration-to-minutes s2)
			      (org-duration-to-minutes s1)))))

	(when (string-match "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$" txt)
	  ;; Tags are in the string
	  (if (or (eq org-agenda-remove-tags t)
		  (and org-agenda-remove-tags
		       org-prefix-has-tag))
	      (setq txt (replace-match "" t t txt))
	    (setq txt (replace-match
		       (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			       (match-string 2 txt))
		       t t txt))))

	(when remove-re
	  (while (string-match remove-re txt)
	    (setq txt (replace-match "" t t txt))))

	;; Set org-heading property on `txt' to mark the start of the
	;; heading.
	(add-text-properties 0 (length txt) '(org-heading t) txt)

	;; Prepare the variables needed in the eval of the compiled format
	(if org-prefix-has-breadcrumbs
	    (setq breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker)
				(let ((s (org-display-outline-path nil nil "->" t)))
				  (if (eq "" s) "" (concat s "->"))))))
	(setq time (cond (s2 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      "-" (org-agenda-time-of-day-to-ampm-maybe s2)
			      (if org-agenda-timegrid-use-ampm " ")))
			 (s1 (concat
			      (org-agenda-time-of-day-to-ampm-maybe s1)
			      (if org-agenda-timegrid-use-ampm
                                  (concat time-grid-trailing-characters " ")
                                time-grid-trailing-characters)))
			 (t ""))
	      extra (or (and (not habitp) extra) "")
	      category (if (symbolp category) (symbol-name category) category)
	      level (or level ""))
	(if (string-match org-bracket-link-regexp category)
	    (progn
	      (setq l (if (match-end 3)
			  (- (match-end 3) (match-beginning 3))
			(- (match-end 1) (match-beginning 1))))
	      (when (< l (or org-prefix-category-length 0))
		(setq category (copy-sequence category))
		(org-add-props category nil
		  'extra-space (make-string
				(- org-prefix-category-length l 1) ?\ ))))
	  (if (and org-prefix-category-max-length
		   (>= (length category) org-prefix-category-max-length))
	      (setq category (substring category 0 (1- org-prefix-category-max-length)))))
	;; Evaluate the compiled format
	(setq rtn (concat (eval formatter) txt))

	;; And finally add the text properties
	(remove-text-properties 0 (length rtn) '(line-prefix t wrap-prefix t) rtn)
	(org-add-props rtn nil
	  'org-category category
	  'tags (mapcar 'org-downcase-keep-props tags)
	  'org-highest-priority org-highest-priority
	  'org-lowest-priority org-lowest-priority
	  'time-of-day time-of-day
	  'duration duration
	  'breadcrumbs breadcrumbs
	  'txt txt
	  'level level
	  'time time
	  'extra extra
	  'format org-prefix-format-compiled
	  'dotime dotime)))))

(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (let* ((org-tags-match-list-sublevels
	  org-tags-match-list-sublevels)
	 (completion-ignore-case t)
	 (org--matcher-tags-todo-only todo-only)
	 rtn rtnall files file pos matcher
	 buffer)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (catch 'exit
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(if (stringp match)
		    (format "*Org Agenda(%s:%s)*"
			    (or org-keys (or (and todo-only "M") "m")) match)
		  (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
      ;; Prepare agendas (and `org-tag-alist-for-agenda') before
      ;; expanding tags within `org-make-tags-matcher'
      (org-agenda-prepare (concat "TAGS " match))
      (setq matcher (org-make-tags-matcher match)
	    match (car matcher)
	    matcher (cdr matcher))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
	    (list 'org-tags-view
		  `(quote ,org--matcher-tags-todo-only)
		  `(if current-prefix-arg nil ,org-agenda-query-string)))
      (setq files (org-agenda-files nil 'ifmode)
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (if (not buffer)
	      ;; If file does not exist, error message to agenda
	      (setq rtn (list
			 (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		    rtnall (append rtnall rtn))
	    (with-current-buffer buffer
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in Org mode" file))
	      (save-excursion
		(save-restriction
		  (if (eq buffer org-agenda-restrict)
		      (narrow-to-region org-agenda-restrict-begin
					org-agenda-restrict-end)
		    (widen))
		  (setq rtn (org-scan-tags 'agenda
					   matcher
					   org--matcher-tags-todo-only))
		  (setq rtnall (append rtnall rtn))))))))
      (when rtnall
      	(if org-agenda-overriding-header
      	    (insert (org-add-props (copy-sequence org-agenda-overriding-header)
      			nil 'face 'org-agenda-structure) "\n")
      	  (setq pos (point))
      	  (insert (upcase match) "\n")
      	  (add-text-properties pos (1- (point)) (list 'face 'org-level-1))
      	  (setq pos (point))
      	  (add-text-properties pos (1- (point))
      			       (list 'face 'org-agenda-structure)))
      	(org-agenda-mark-header-line (point-min))
      	(when rtnall
      	  (insert (org-agenda-finalize-entries rtnall 'tags) "\n"))
      	(goto-char (point-min))
      	(or org-agenda-multi (org-agenda-fit-window-to-buffer))
      	(add-text-properties
      	 (point-min) (point-max)
      	 `(org-agenda-type tags
      			   org-last-args (,org--matcher-tags-todo-only ,match)
      			   org-redo-cmd ,org-agenda-redo-command
      			   org-series-cmd ,org-cmd)))
      (org-agenda-finalize)
      (setq buffer-read-only t))))


(provide 'my-org)
