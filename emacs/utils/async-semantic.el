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

(defvar async-semantic--cb nil)

(defvar async-semantic--files-parsed nil)

(defvar async-semantic--files-up-to-date nil)

(defvar async-semantic--files-not-found nil)

;;; Internal Functions

(defun async-semantic--save (path files)
  (let ((save-silently t))
    (with-current-buffer (find-file-noselect path)
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
  (if (eq (process-exit-status process) 0)
      (when async-semantic--cb
	(let ((files-parsed (async-semantic--read async-semantic-files-parsed))
	      (files-up-to-date (async-semantic--read async-semantic-files-up-to-date)))
	  (funcall async-semantic--cb files-parsed files-up-to-date))
	(kill-buffer (process-buffer process)))
    (message (concat "Async Semantic Database: "
		     (propertize "Failed" 'face 'error)))
    (switch-to-buffer-other-window (process-buffer process)))
  (delete-file async-semantic-files)
  (delete-file async-semantic-includes)
  (delete-file async-semantic-files-parsed)
  (delete-file async-semantic-files-up-to-date))

(defun async-semantic--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert str))))

(defun async-semantic--get-cache (file)
  (let* ((dir (file-name-directory (file-truename file)))
	 (cache-file (semanticdb-cache-filename semanticdb-new-database-class
						dir)))
    (when (file-exists-p cache-file)
      cache-file)))

(defun async-semantic--get-table (file)
  (when-let* ((filename (file-name-nondirectory file))
	      (cache-file (async-semantic--get-cache file))
	      (db (semanticdb-load-database cache-file))
	      (table (seq-find (lambda (table)
				 (string= filename (oref table file)))
			       (oref db tables))))
    (delete-instance db)
    table))

(defun async-semantic--includes (file)
  (when-let* ((cur-dir (file-name-directory file))
	      (paths (append (list cur-dir) semantic-default-c-path))
	      (table (async-semantic--get-table file))
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
    (semanticdb-save-current-db))
  (add-to-list 'async-semantic--files-parsed file))

(defun async-semantic--up-to-date-p (file)
  (when-let* ((table (async-semantic--get-table file))
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
      (dolist (include (async-semantic--includes file))
	(async-semantic--parse include recursive)))))

;;; External Functions

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

(defun async-semantic (files &optional cb recursive includes)
  (unless (async-semantic-parse-running)
    (setq async-semantic--cb cb)
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

(defun async-semantic-buffer (&optional cb recursive includes)
  (interactive)
  (async-semantic (list (buffer-file-name)) cb recursive includes))

(provide 'async-semantic)
