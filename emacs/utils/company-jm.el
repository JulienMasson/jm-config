;;; company-jm.el --- Custom company-mode completion backend

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

(require 'async-semantic-db)
(require 'semantic/db-mode)

;;; Customization

(defcustom company-jm-modes '(c-mode c++-mode)
  "List of mode when `company-jm' is enabled"
  :type 'list
  :group 'company)

(defcustom company-jm-enabled t
  "Enable/Disable `company-jm', by default it's enabled"
  :type 'boolean
  :group 'company)


;;; Internal Variables

(defvar-local company-jm--tags nil)

(defvar-local company-jm--files nil)

;;; Internal Functions

(defun company-jm--reset ()
  (setq company-jm--tags nil)
  (setq company-jm--files nil))

(defun company-jm--buffer-parsed ()
  (let* ((dd (file-name-directory (file-truename (buffer-file-name))))
	 (cache-file (semanticdb-cache-filename semanticdb-new-database-class dd)))
    (file-exists-p cache-file)))

(defun company-jm--get-includes (tags)
  (when-let ((includes (seq-filter (lambda (tag)
				     (eq (semantic-tag-class tag) 'include))
				   tags)))
    (mapcar #'semantic-dependency-tag-file includes)))

(defun company-jm--fetch-tags (file)
  (when (and file (not (member file company-jm--files)))
    (add-to-list 'company-jm--files file)
    (when-let* ((filename (file-name-nondirectory (file-truename file)))
		(dir (file-name-directory (file-truename file)))
		(cache-file (semanticdb-cache-filename semanticdb-new-database-class dir))
		(db (semanticdb-load-database cache-file))
		(table (seq-find (lambda (table)
				   (string= filename (oref table file)))
				 (oref db tables)))
		(tags (oref table tags)))
      (setq company-jm--tags (append company-jm--tags tags))
      (mapc #'company-jm--fetch-tags (company-jm--get-includes tags)))))

(defun company-jm--completions (prefix)
  (when-let ((functions (seq-filter (lambda (tag)
				      (eq (semantic-tag-class tag) 'function))
				    company-jm--tags)))
    (seq-filter (lambda (func)
		  (string-prefix-p prefix func))
		(mapcar #'car functions))))

(defun company-jm--completions-raw (prefix)
  nil)

(defun company-jm--candidates (arg)
  (let (candidates)
    (if (not (company-jm--buffer-parsed))
	(async-semantic-db-buffer)
      (unless company-jm--tags
	(setq company-jm--files nil)
	(company-jm--fetch-tags (buffer-file-name)))
      (setq candidates (if (and (equal arg "")
				(not (looking-back "->\\|\\.\\|::" (- (point) 2))))
			   (company-jm--completions-raw arg)
			 (company-jm--completions arg))))
    candidates))

(defun company-jm--prefix ()
  (and company-jm-enabled
       (memq major-mode company-jm-modes)
       (not (company-in-string-or-comment))
       (not async-semantic-db--ongoing)
       (or (company-grab-symbol-cons "\\.\\|->\\|::" 2) 'stop)))

(defun company-jm--buffer-list ()
  (seq-filter (lambda (buffer)
		(with-current-buffer buffer
		  (and company-jm-enabled
		       (memq major-mode company-jm-modes))))
	      (buffer-list)))

(defmacro foreach-company-jm-buffer (&rest body)
  (declare (indent 2))
  `(mapc (lambda (buffer)
	   (with-current-buffer buffer
	     ,@body))
	 (company-jm--buffer-list)))

;;; External Functions

(defun company-jm-toggle ()
  (interactive)
  (setq company-jm-enabled (not company-jm-enabled))
  (message (concat "Company JM: " (if company-jm-enabled
				      (propertize "Enabled" 'face 'success)
				    (propertize "Disabled" 'face 'error)))))

(defun company-jm (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-jm))
    (prefix (company-jm--prefix))
    (candidates (company-jm--candidates arg))
    (duplicates t)))

(provide 'company-jm)
