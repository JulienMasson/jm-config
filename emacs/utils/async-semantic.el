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

;;; Internal Variables

(defvar async-semantic--ongoing nil)

(defvar async-semantic--cb nil)

(defvar async-semantic--files nil)

(defvar async-semantic--files-parsed nil)

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
      (let ((files (async-semantic--read async-semantic-files-parsed)))
	(when (and files async-semantic--cb)
	  (funcall async-semantic--cb files))
	(kill-buffer (process-buffer process)))
    (message (concat "Async Semantic Database: "
		     (propertize "Failed" 'face 'error)))
    (switch-to-buffer-other-window (process-buffer process)))
  (delete-file async-semantic-files)
  (delete-file async-semantic-includes)
  (delete-file async-semantic-files-parsed)
  (setq async-semantic--ongoing nil)
  (setq async-semantic--cb nil))

(defun async-semantic--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert str))))

(defun async-semantic--files-p (file)
  (and file (not (member file async-semantic--files))))

(defun async-semantic--parse (file)
  (when (async-semantic--files-p file)
    (add-to-list 'async-semantic--files file)
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
	;; parse current file
	(message "Parsing: %s" file)
	(setq-mode-local c-mode
  			 semantic-dependency-system-include-path
  			 semantic-default-c-path)
	(semanticdb-save-current-db)
	(add-to-list 'async-semantic--files-parsed file)))))

(defun async-semantic--parse-recursive (file)
  (when (async-semantic--files-p file)
    (add-to-list 'async-semantic--files file)
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
	;; parse current file
	(message "Parsing: %s" file)
	(setq-mode-local c-mode
  			 semantic-dependency-system-include-path
  			 semantic-default-c-path)
	(semanticdb-save-current-db)
	(add-to-list 'async-semantic--files-parsed file)
	;; parse include files
	(mapc #'async-semantic--parse-recursive
	      (mapcar (lambda (include)
			(if-let ((name (car include))
				 (path (semantic-dependency-tag-file include)))
			    path
			  (when (async-semantic--files-p name)
			    (message "\n-> Cannot find path: %s\n" name))
			  name))
		      (semantic-find-tags-included (current-buffer))))))))

;;; External Functions

(defun async-semantic-parse (files-path includes-path &optional recursive)
  (let ((files (async-semantic--read files-path))
	(includes (async-semantic--read includes-path)))
    (setq semantic-default-c-path includes)
    (message "-> Includes:\n%s\n" semantic-default-c-path)
    (semantic-mode)
    (if recursive
	(mapc #'async-semantic--parse-recursive files)
      (mapc #'async-semantic--parse files))
    (async-semantic--save async-semantic-files-parsed async-semantic--files-parsed)))

(defun async-semantic (files &optional cb recursive includes)
  (unless async-semantic--ongoing
    (setq async-semantic--ongoing t)
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
      (with-current-buffer buffer
	(erase-buffer))
      (set-process-filter process 'async-semantic--process-filter)
      (set-process-sentinel process 'async-semantic--process-sentinel))))

(defun async-semantic-buffer (&optional cb recursive includes)
  (interactive)
  (async-semantic (list (buffer-file-name)) cb recursive includes))

(provide 'async-semantic)
