;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ORG CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode and modules
(require 'org)
(require 'org-agenda)
(require 'org-gtasks)
(require 'org-gcal)

;; Add a time stamp when a task moves to the DONE state.
(setq org-log-done t)

;; clear files to be used for agenda display
(setq org-agenda-files nil)

;; org todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "UNMERGED" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("WORKING" . "yellow")
	("UNMERGED" . (:foreground "lightblue" :weight bold))))

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
(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq todo-only (car org-agenda-overriding-arguments)
	    match (nth 1 org-agenda-overriding-arguments)))
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
      (if org-agenda-overriding-header
	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
		      nil 'face 'org-agenda-structure) "\n")
	(setq pos (point))
	(insert (upcase match) "\n")
	(add-text-properties pos (1- (point)) (list 'face 'org-level-1))
	(setq pos (point))
	(unless org-agenda-multi
	  (insert (substitute-command-keys
		   "Press `\\[universal-argument] \\[org-agenda-redo]' \
to search again with new search string\n")))
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
			 org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))


(provide 'my-org)
