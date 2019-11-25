;;; async-semantic-db.el --- Asynchronous Semantic Database Management

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

;;; External Variables

(defconst async-semantic-db-files (concat semanticdb-default-save-directory
					  "/.async-semantic-db-files"))

(defconst async-semantic-db-includes (concat semanticdb-default-save-directory
					     "/.async-semantic-db-includes"))

;;; Internal Variables

(defvar async-semantic-db--ongoing nil)

(defvar async-semantic-db--files-parsed nil)

;;; Internal Functions

(defun async-semantic-db--save (path files)
  (let ((save-silently t))
    (with-current-buffer (find-file-noselect path)
      (erase-buffer)
      (mapc (lambda (file)
	      (insert (concat file "\n")))
	    files)
      (save-buffer)
      (kill-current-buffer))
    (message nil)))

(defun async-semantic-db--read (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun async-semantic-db--get-args ()
  (list "--quick" "--batch"
	"--load" (locate-library "async-semantic-db")
	"--eval" (format "(async-semantic-db-parse \"%s\" \"%s\")"
			 async-semantic-db-files
			 async-semantic-db-includes)))

(defun async-semantic-db--process-sentinel (process status)
  (if (eq (process-exit-status process) 0)
      (kill-buffer (process-buffer process))
    (message (concat "Async Semantic Database: "
		     (propertize "Failed" 'face 'error)))
    (switch-to-buffer-other-window (process-buffer process)))
  (delete-file async-semantic-db-files)
  (delete-file async-semantic-db-includes)
  (setq async-semantic-db--ongoing nil))

(defun async-semantic-db--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

(defun async-semantic-db--parse-recursive (file)
  (when (and file (not (member file async-semantic-db--files-parsed)))
    (with-current-buffer (find-file-noselect file)
      ;; parse current file
      (message "Parsing: %s" file)
      (semanticdb-save-current-db)
      (add-to-list 'async-semantic-db--files-parsed file)
      ;; parse include files
      (mapc #'async-semantic-db--parse-recursive
	    (mapcar (lambda (include)
		      (cl-multiple-value-bind (name _ system tag)
			  include
			(let ((system (plist-get system :system-flag))
			      (src (plist-get tag :filename)))
			  (cond ((and system tag)
		      		 (plist-get tag 'dependency-file))
				((and (not system) src)
				 (concat (file-name-directory src) name))
				(t (concat default-directory name))))))
		    (semantic-find-tags-included (current-buffer)))))))

;;; External Functions

(defun async-semantic-db-parse (files-path includes-path)
  (let ((files (async-semantic-db--read files-path))
	(includes (async-semantic-db--read includes-path)))
    (setq semantic-default-c-path includes)
    (setq-mode-local c-mode
  		     semantic-dependency-system-include-path
  		     semantic-default-c-path)
    (message "-> Includes:\n%s\n" semantic-default-c-path)
    (semantic-mode)
    (mapc #'async-semantic-db--parse-recursive files)
    (semanticdb-save-all-db)))

(defun async-semantic-db (files includes)
  (unless async-semantic-db--ongoing
    (setq async-semantic-db--ongoing t)
    (async-semantic-db--save async-semantic-db-files files)
    (async-semantic-db--save async-semantic-db-includes includes)
    (let* ((default-directory (getenv "HOME"))
	   (program (executable-find "emacs"))
	   (args (async-semantic-db--get-args))
	   (buffer (get-buffer-create "*async-semantic-db*"))
	   (process (apply 'start-process "async-semantic-db" buffer
			   program args)))
      (with-current-buffer buffer
	(erase-buffer))
      (set-process-filter process 'async-semantic-db--process-filter)
      (set-process-sentinel process 'async-semantic-db--process-sentinel))))

(defun async-semantic-db-buffer ()
  (interactive)
  (async-semantic-db (list (buffer-file-name)) semantic-default-c-path))

(provide 'async-semantic-db)
