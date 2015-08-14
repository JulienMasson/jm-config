;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump only on error compilation
(setq compilation-skip-threshold 2)

;; cscope
(require 'xcscope)
(cscope-setup)
(setq path-to-cscope "/usr/bin/cscope")
(defun create-tags-cscope (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "cd %s; find . -name \"*.c\" -o -name \"*.cpp\" -o -name \"*.h\" > cscope.files; %s -q -R -b -i cscope.files" (directory-file-name dir-name) path-to-cscope)))

;; cscope database management
(defvar cscope-database-cmds
  '(("add"			.       jm-add-cscope-database)
    ("clean"			.       jm-clean-cscope-database)
    ("update"			.       jm-update-cscope-database)
    ("update-all"		.       jm-update-all-cscope-database)
    ("current"			.       jm-current-cscope-database)
    ("switch"		        .	jm-switch-cscope-database)))

(defvar list-cscope-database '())
(defvar current-cscope-database nil)

(defun jm-add-cscope-database (dir)
  (interactive "DFrom: ")
  (let ((default-directory dir)
	(cscope-database (split-string
				(shell-command-to-string "find . -name cscope.files")
				"\n" t)))
    (dolist (index cscope-database)
      (setq element-path (replace-regexp-in-string "cscope.files" "" index))
      (if (string= element-path "./")
	  (setq element-path dir)
	(setq element-path (concat dir element-path)))
      (setq element (car (last (split-string element-path "/" t))))
      (add-to-list 'list-cscope-database `( ,element . ,element-path )))))

(defun jm-clean-cscope-database ()
  (interactive)
  (dolist (index list-cscope-database)
    (setq list-cscope-database (delete index list-cscope-database)))
  (setq current-cscope-database nil)
  (cscope-unset-initial-directory)
  (message (propertize "Clean done" 'face 'success)))

(defun jm-update-cscope-database (database)
  (interactive (list (ido-completing-read "Update database: "
					  (mapcar 'car list-cscope-database)
					  nil nil nil nil)))
  (if (mapcar 'car list-cscope-database)
      (progn
	(cscope-index-files (assoc-default database list-cscope-database))
    	(message (concat "Update database: " (propertize (format "%s" database) 'face 'success))))
    (message (propertize "No database" 'face 'error))))

(defun jm-update-all-cscope-database ()
  (interactive)
  (if (mapcar 'car list-cscope-database)
      (progn
	(dolist (index list-cscope-database)
	  (cscope-index-files (assoc-default index list-cscope-database)))
	(message (propertize "Update all database" 'face 'success)))
    (message (propertize "No database" 'face 'error))))

(defun jm-current-cscope-database ()
  (interactive)
  (if current-cscope-database
      (message (concat "Current database: " (propertize (format "%s" current-cscope-database) 'face 'success)))
    (message (propertize "No database selected" 'face 'error))))

(defun jm-switch-cscope-database (database)
  (interactive (list (ido-completing-read "Switch to database: "
					  (mapcar 'car list-cscope-database)
					  nil nil nil nil)))
  (if (mapcar 'car list-cscope-database)
      (progn
	(cscope-set-initial-directory (assoc-default database list-cscope-database))
	(setq current-cscope-database database)
	(message (concat "Switch to: " (propertize (format "%s" current-cscope-database) 'face 'success))))
    (message (propertize "No database" 'face 'error))))

(defun jm-cscope-database (cmds)
    (interactive (list (ido-completing-read "Cscope database: "
					  (mapcar 'car cscope-database-cmds)
					  nil t nil nil)))
      (let ((t-or-f (assoc-default cmds cscope-database-cmds)))
	(if (functionp t-or-f)
	    (call-interactively t-or-f))))

;; auto-detection indenting
(require 'dtrt-indent)
(add-hook 'c-mode-common-hook
	  (lambda() (require 'dtrt-indent)
	    (dtrt-indent-mode t)))

;; change default grep
(setq grep-command "grep -nrH -e ")

;; auto complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/jm-config/auto-complete/dict/")
(ac-config-default)

;; auto complete based on ggtags
(require 'setup-ggtags)
(set-face-foreground 'ac-gtags-candidate-face "coral3")
(set-face-background 'ac-gtags-selection-face nil)

;; locate database
(defvar jm-cmd-locate-database
  '(("current"			.       jm-current-locate-database)
    ("set"			.       jm-set-locate-database)
    ("update"			.       jm-update-locate-database)
    ("search"		        .	jm-search-locate-database)))

(setq locate-database-path nil)
(setq jm-locate-history nil)

(defun jm-current-locate-database ()
  (interactive)
  (if locate-database-path
      (message (concat "Database path: " (propertize locate-database-path 'face 'success)))
    (message (concat "Database path: " (propertize "not found" 'face 'error)))))

(defun jm-set-locate-database (dir)
  (interactive "DFrom: ")
  (setq locate-database-path (concat dir (car (last (split-string dir "/" t))) ".db"))
  (if (file-exists-p locate-database-path)
      (message (propertize "Found database" 'face 'success))
    (progn
      (shell-command (format "updatedb -U %s -o %s --prunenames \".bzr .hg .git .svn\" -l 0"
			     (untramp-path dir)
			     (untramp-path locate-database-path)))
      (message (propertize "Create database" 'face 'success)))))

(defun jm-update-locate-database ()
  (interactive)
  (if locate-database-path
      (let ((default-directory (file-name-directory locate-database-path)))
	(shell-command (format "updatedb -U %s -o %s --prunenames \".bzr .hg .git .svn\" -l 0"
			     (untramp-path default-directory)
			     (untramp-path locate-database-path)))
	(message (propertize "Update database" 'face 'success)))
    (message (propertize "No database selected" 'face 'error))))

(defun jm-search-locate-database (pattern)
  (interactive (list (read-string "Pattern: "
				  nil 'jm-locate-history
				  (car jm-locate-history))))
  (if locate-database-path
      (let ((dired-buffers dired-buffers))
	;; Expand DIR ("" means default-directory), and make sure it has a
	;; trailing slash.
	(setq dir (file-name-directory locate-database-path))
	(switch-to-buffer (get-buffer-create "*Locate*"))

	;; See if there's still a `locate' running, and offer to kill
	;; it first, if it is.
	(let ((locate (get-buffer-process (current-buffer))))
	  (when locate
	    (if (or (not (eq (process-status locate) 'run))
		    (yes-or-no-p "A `locate' process is running; kill it? "))
		(condition-case nil
		    (progn
		      (interrupt-process locate)
		      (sit-for 1)
		      (delete-process locate))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once" (buffer-name)))))
	(widen)
	(kill-all-local-variables)
	(setq buffer-read-only nil)
	(erase-buffer)
	(setq default-directory dir
	      args (concat "locate -d " (car (last (split-string locate-database-path ":" t))) " " pattern))
	;; Start the locate process.
	(shell-command (concat args (format " | xargs -r ls -dilsb | sed 's,%s,,g'" (untramp-path (file-name-directory locate-database-path))))
		       (current-buffer))
	;; The next statement will bomb in classic dired (no optional arg allowed)
	(dired-mode dir "-dilsb")
	(make-local-variable 'dired-sort-inhibit)
	(setq dired-sort-inhibit t)
	(set (make-local-variable 'revert-buffer-function)
	     `(lambda (ignore-auto noconfirm)
		(jm-update-locate-database)
		(jm-search-locate-database ,pattern)))
	;; Set subdir-alist so that Tree Dired will work:
	(if (fboundp 'dired-simple-subdir-alist)
	    ;; will work even with nested dired format (dired-nstd.el,v 1.15
	    ;; and later)
	    (dired-simple-subdir-alist)
	  ;; else we have an ancient tree dired (or classic dired, where
	  ;; this does no harm)
	  (set (make-local-variable 'dired-subdir-alist)
	       (list (cons default-directory (point-min-marker)))))
	(set (make-local-variable 'dired-subdir-switches) "-alb")
	(setq buffer-read-only nil)
	;; Subdir headlerline must come first because the first marker in
	;; subdir-alist points there.
	(insert "  " dir ":\n")
	;; Make second line a ``find'' line in analogy to the ``total'' or
	;; ``wildcard'' line.
	(let ((point (point)))
	  (insert "  " args "\n")
	  (dired-insert-set-properties point (point)))
	(setq buffer-read-only t)
	(let ((proc (get-buffer-process (current-buffer))))
	  (set-process-filter proc (function find-dired-filter))
	  (set-process-sentinel proc (function find-dired-sentinel))
	  ;; Initialize the process marker; it is used by the filter.
	  (move-marker (process-mark proc) (point) (current-buffer)))
	(setq mode-line-process '(":%s")))
    (message (propertize "No database selected" 'face 'error))))

(defun jm-locate-database (cmds)
    (interactive (list (ido-completing-read "Locate database: "
					  (mapcar 'car jm-cmd-locate-database)
					  nil t nil nil)))
      (let ((t-or-f (assoc-default cmds jm-cmd-locate-database)))
	(if (functionp t-or-f)
	    (call-interactively t-or-f))))


(provide 'my-programming)
