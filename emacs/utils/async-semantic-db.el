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

;;; External Variables

(defconst async-semantic-db-files (concat semanticdb-default-save-directory
					  "/.async-semantic-db-files"))

;;; Internal Variables

(defvar async-semantic-db--ongoing nil)

;;; Internal Functions

(defun async-semantic-db--parse-init ()
  (setq semantic-default-c-path '("/usr/include/" "/usr/local/include/"
				  "/usr/include/x86_64-linux-gnu/"))
  (semantic-mode))

(defun async-semantic-db--add-files (files)
  (let ((save-silently t))
    (with-current-buffer (find-file-noselect async-semantic-db-files)
      (erase-buffer)
      (mapc (lambda (file)
	      (insert (concat file "\n")))
	    files)
      (save-buffer)
      (kill-current-buffer))
    (message nil)))

(defun async-semantic-db--get-args ()
  (list "--quick" "--batch"
	"--load" (locate-library "async-semantic-db")
	"--eval" (format "(async-semantic-db-parse \"%s\")"
			 async-semantic-db-files)))

(defun async-semantic-db--process-sentinel (process status)
  (if (eq (process-exit-status process) 0)
      (kill-buffer (process-buffer process))
    (message (concat "Async Semantic Database: "
		     (propertize "Failed" 'face 'error)))
    (switch-to-buffer-other-window (process-buffer process)))
  (delete-file async-semantic-db-files)
  (setq async-semantic-db--ongoing nil))

(defun async-semantic-db--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

;;; External Functions

(defun async-semantic-db-parse (files-list)
  (async-semantic-db--parse-init)
  (mapc (lambda (file)
	  (message "Parsing: %s" file)
	  (with-current-buffer (find-file-noselect file)
	    (semanticdb-save-current-db)))
	(with-temp-buffer
	  (insert-file-contents files-list)
	  (split-string (buffer-string) "\n" t))))

(defun async-semantic-db (files)
  (unless async-semantic-db--ongoing
    (setq async-semantic-db--ongoing t)
    (async-semantic-db--add-files files)
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
  (async-semantic-db (list (buffer-file-name))))

(provide 'async-semantic-db)
