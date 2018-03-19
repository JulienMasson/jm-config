;;; locate-database.el

;; Copyright (C) 2017 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>

;; This file is NOT part of GNU Emacs.

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

;; required modules
(require 'dired)
(require 'intel-lib)

;; locate database
(defvar cmd-locate-database
  '(("current"			.       current-locate-database)
    ("set"			.       set-locate-database)
    ("update"			.       update-locate-database)
    ("search"		        .	search-locate-database)))

(defvar locate-database-path nil)
(defvar locate-history nil)

(defun current-locate-database ()
  (interactive)
  (if locate-database-path
      (message (concat "Database path: " (propertize locate-database-path 'face 'success)))
    (message (concat "Database path: " (propertize "not found" 'face 'error)))))

(defun set-locate-database (dir)
  (interactive "DFrom: ")
  (setq locate-database-path (concat (expand-file-name dir) "locate.db"))
  (if (file-exists-p locate-database-path)
      (message (propertize "Found database" 'face 'success))
    (progn
      (shell-command (format "updatedb --database-root=%s --output=%s --prunenames=\".bzr .hg .git .svn .repo\""
			     (untramp-path (expand-file-name dir))
			     (untramp-path locate-database-path)))
      (message (propertize "Create database" 'face 'success)))))

(defun update-locate-database ()
  (interactive)
  (if locate-database-path
      (let ((default-directory (file-name-directory locate-database-path)))
	(shell-command (format "updatedb --localpaths=%s --output=%s --prunefs=\".bzr .hg .git .svn\""
			     (untramp-path default-directory)
			     (untramp-path locate-database-path)))
	(message (propertize "Update database" 'face 'success)))
    (message (propertize "No database selected" 'face 'error))))

(defun search-locate-database (pattern)
  (interactive (list (read-string "Pattern: "
				  nil 'locate-history
				  (car locate-history))))
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
	      args (format "locate --regex --database=%s \"%s\""
			   locate-database-path
			   pattern))
	;; Start the locate process.
	(shell-command (concat args (format " | xargs -r ls -dilsb | sed 's,%s,,g'" (untramp-path (file-name-directory locate-database-path))))
		       (current-buffer))
	;; The next statement will bomb in classic dired (no optional arg allowed)
	(dired-mode dir "-dilsb")
	(make-local-variable 'dired-sort-inhibit)
	(setq dired-sort-inhibit t)
	(set (make-local-variable 'revert-buffer-function)
	     `(lambda (ignore-auto noconfirm)
		(update-locate-database)
		(search-locate-database ,pattern)))
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

(defun locate-database (cmds)
    (interactive (list (ido-completing-read "Locate database: "
					  (mapcar 'car cmd-locate-database)
					  nil t nil nil)))
      (let ((t-or-f (assoc-default cmds cmd-locate-database)))
	(if (functionp t-or-f)
	    (call-interactively t-or-f))))


(provide 'locate-database)
