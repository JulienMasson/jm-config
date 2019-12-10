;;; async-semantic.el --- Asynchronous Semantic Database Management

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

(require 'semantic)
(require 'semantic/db-file)
(require 'subr-x)

;;; Customization

(defcustom async-semantic-default-path '("/usr/include/" "/usr/local/include/")
  "Default path use when parsing files"
  :type 'list
  :group 'semantic)

;;; External Variables

(defconst async-semantic-files (concat semanticdb-default-save-directory
				       "/.async-semantic-files"))

(defconst async-semantic-includes (concat semanticdb-default-save-directory
					  "/.async-semantic-includes"))

(defconst async-semantic-files-parsed (concat semanticdb-default-save-directory
					      "/.async-semantic-files-parsed"))

(defconst async-semantic-files-up-to-date (concat semanticdb-default-save-directory
						  "/.async-semantic-files-up-to-date"))

;;; Internal Variables

(defvar async-semantic--process nil)

(defvar async-semantic--cb-success nil)

(defvar async-semantic--cb-fail nil)

(defvar async-semantic--files-parsed nil)

(defvar async-semantic--files-up-to-date nil)

(defvar async-semantic--files-not-found nil)

(defvar async-semantic--cache-databases nil)

;;; Internal Functions

(defun async-semantic--remote-host (file)
  (replace-regexp-in-string "\\(^/ssh:.*:\\).*" "\\1" file))

(defun async-semantic--remote-locate-file (file paths)
  (let (match)
    (catch 'match
      (dolist (path paths)
	(when (file-exists-p path)
	  (let* ((default-directory path)
		 (host (async-semantic--remote-host path))
		 (local-dir (replace-regexp-in-string host "" path))
		 (program (executable-find "find")))
	    (with-temp-buffer
	      (process-file program nil (current-buffer) nil
			    local-dir "-name" file)
	      (goto-char (point-min))
	      (unless (= (point-min) (point-max))
		(setq match (concat host (buffer-substring-no-properties
					  (line-beginning-position)
					  (line-end-position)))))))))
      match)))

(defun async-semantic--save (path files)
  (let ((save-silently t)
	(message-log-max nil))
    (with-current-buffer (find-file-noselect path t)
      (erase-buffer)
      (mapc (lambda (file)
	      (insert (concat file "\n")))
	    files)
      (save-buffer)
      (kill-current-buffer))
    (message nil)))

(defun async-semantic--read (path)
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (split-string (buffer-string) "\n" t))))

(defun async-semantic--get-args (recursive)
  (list "--quick" "--batch"
	"--load" (locate-library "async-semantic")
	"--eval" (concat "(async-semantic-parse "
			 (format "\"%s\" \"%s\" " async-semantic-files
				 async-semantic-includes)
			 (if recursive "t")
			 ")")))

(defun async-semantic--process-sentinel (process status)
  (let ((buffer (process-buffer process)))
    (if (eq (process-exit-status process) 0)
	(progn
	  (when async-semantic--cb-success
	    (let ((files-parsed (async-semantic--read async-semantic-files-parsed))
		  (files-up-to-date (async-semantic--read async-semantic-files-up-to-date)))
	      (funcall async-semantic--cb-success files-parsed files-up-to-date)))
	  (kill-buffer buffer))
      (message (concat "Async Semantic Database: "
		       (propertize "Failed" 'face 'error)))
      (when (buffer-live-p buffer)
	(switch-to-buffer-other-window buffer))
      (when async-semantic--cb-fail
	(funcall async-semantic--cb-fail))))
  (delete-file async-semantic-files)
  (delete-file async-semantic-includes)
  (delete-file async-semantic-files-parsed)
  (delete-file async-semantic-files-up-to-date))

(defun async-semantic--process-filter (process str)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (insert str))))))

(defun async-semantic--get-cache (file)
  (let* ((dir (file-name-directory (file-truename file)))
	 (cache-file (semanticdb-cache-filename semanticdb-new-database-class
						dir)))
    (when (file-exists-p cache-file)
      cache-file)))

(defun async-semantic--fast-find-table (filename db)
  (seq-find (lambda (table)
	      (string= filename (oref table file)))
	    (oref db tables)))

(defun async-semantic--fast-get-table (file)
  (if-let* ((filename (file-name-nondirectory file))
	    (cache-file (async-semantic--get-cache file))
	    (db (assoc-default cache-file async-semantic--cache-databases)))
      (async-semantic--fast-find-table filename db)
    (when cache-file
      (setq db (semanticdb-load-database cache-file))
      (push (cons cache-file db) async-semantic--cache-databases)
      (async-semantic--fast-find-table filename db))))

(defun async-semantic--get-table (file)
  (when-let* ((filename (file-name-nondirectory file))
	      (cache-file (async-semantic--get-cache file))
	      (db (semanticdb-load-database cache-file))
	      (table (seq-find (lambda (table)
				 (string= filename (oref table file)))
			       (oref db tables))))
    (delete-instance db)
    table))

(defun async-semantic--fast-get-includes (file default-path)
  (when-let* ((cur-dir (file-name-directory file))
	      (paths (append (list cur-dir) default-path))
	      (filename (file-name-nondirectory file))
	      (cache-file (async-semantic--get-cache file))
	      (db (assoc-default cache-file async-semantic--cache-databases))
	      (table (async-semantic--fast-find-table filename db))
	      (tags (oref table tags))
	      (includes (seq-filter (lambda (tag)
				      (eq (semantic-tag-class tag) 'include))
				    tags)))
    (delq nil (mapcar (lambda (include)
			(locate-file (semantic-tag-name include) paths))
		      includes))))

(defun async-semantic--message (header str)
  (message "%-15s %s" header str))

(defun async-semantic--parse-file (file)
  (with-current-buffer (find-file-noselect file)
    (async-semantic--message "Parsing:" file)
    (setq-mode-local c-mode
  		     semantic-dependency-system-include-path
  		     semantic-default-c-path)
    (semantic-force-refresh)
    (semanticdb-save-current-db))
  (add-to-list 'async-semantic--files-parsed file))

(defun async-semantic--up-to-date-p (file)
  (when-let* ((table (async-semantic--fast-get-table file))
	      (stats (file-attributes file))
	      (curmodtime (file-attribute-modification-time stats)))
    (and (equal (oref table lastmodtime) curmodtime)
	 (slot-boundp table 'tags))))

(defun async-semantic--parse-p (file)
  (or (member file async-semantic--files-up-to-date)
      (member file async-semantic--files-not-found)
      (member file async-semantic--files-parsed)))

(defun async-semantic--parse (file &optional recursive)
  (unless (async-semantic--parse-p file)
    (cond ((async-semantic--up-to-date-p file)
	   (add-to-list 'async-semantic--files-up-to-date file)
	   (async-semantic--message "Up-To-Date:" file))
	  ((not (file-exists-p file))
	   (add-to-list 'async-semantic--files-not-found file)
	   (async-semantic--message "File not found:" file))
	  (t (async-semantic--parse-file file)))
    (when recursive
      (let* ((path semantic-default-c-path)
	     (includes (if (member file async-semantic--files-up-to-date)
			  (async-semantic--fast-get-includes file path)
			 (async-semantic-get-includes file path))))
	(dolist (include includes)
	  (async-semantic--parse include recursive))))))

;;; External Functions

(defun async-semantic-locate-file (file paths)
  (when-let* ((default-dir (expand-file-name default-directory))
	      (default-paths (append (list default-dir) paths)))
    (if (tramp-tramp-file-p default-dir)
	(async-semantic--remote-locate-file file default-paths)
      (locate-file file default-paths))))

(defun async-semantic-get-includes (file default-path)
  (when-let* ((cur-dir (file-name-directory file))
	      (paths (append (list cur-dir) default-path))
	      (table (async-semantic--get-table file))
	      (tags (oref table tags))
	      (includes (seq-filter (lambda (tag)
				      (eq (semantic-tag-class tag) 'include))
				    tags)))
    (delq nil (mapcar (lambda (include)
			(async-semantic-locate-file (semantic-tag-name include)
						    paths))
		      includes))))

(defun async-semantic-parse-running ()
  (and async-semantic--process
       (eq (process-status async-semantic--process) 'run)))

(defun async-semantic-parse (files-path includes-path &optional recursive)
  (let ((files (async-semantic--read files-path))
	(includes (async-semantic--read includes-path)))
    (setq semantic-default-c-path includes)
    (setq semantic-c-obey-conditional-section-parsing-flag nil)
    (message "-> Default Path:\n%s\n" (mapconcat 'identity semantic-default-c-path "  "))
    (semantic-mode)
    (dolist (file files)
      (async-semantic--parse file recursive))
    (async-semantic--save async-semantic-files-parsed async-semantic--files-parsed)
    (async-semantic--save async-semantic-files-up-to-date async-semantic--files-up-to-date)))

(defun async-semantic (files &optional success fail recursive includes)
  (unless (async-semantic-parse-running)
    (setq async-semantic--cb-success success)
    (setq async-semantic--cb-fail fail)
    (async-semantic--save async-semantic-files files)
    (async-semantic--save async-semantic-includes
			  (if includes
			      includes
			    async-semantic-default-path))
    (let* ((default-directory (getenv "HOME"))
	   (program (executable-find "emacs"))
	   (args (async-semantic--get-args recursive))
	   (buffer (get-buffer-create "*async-semantic*"))
	   (process (apply 'start-process "async-semantic" buffer
			   program args)))
      (setq async-semantic--process process)
      (with-current-buffer buffer
	(erase-buffer))
      (set-process-filter process 'async-semantic--process-filter)
      (set-process-sentinel process 'async-semantic--process-sentinel))))

(defun async-semantic-buffer (&optional success fail recursive includes)
  (interactive)
  (async-semantic (list (buffer-file-name)) success fail recursive includes))

(provide 'async-semantic)
