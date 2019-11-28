;;; company-async-semantic.el --- Custom company-mode completion backend

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

(require 'async-semantic)

;;; Customization

(defcustom company-async-semantic-modes '(c-mode c++-mode)
  "List of mode when `company-async-semantic' is enabled"
  :type 'list
  :group 'company)

(defcustom company-async-semantic-enabled t
  "Enable/Disable `company-async-semantic', by default it's enabled"
  :type 'boolean
  :group 'company)


;;; Internal Variables

(defvar company-async-semantic--cache nil)

(defvar-local company-async-semantic--files-dep nil)

;;; Internal Functions

(defmacro company-async-semantic--find-file-on-path (file path)
  (if (fboundp 'locate-file)
      `(locate-file ,file ,path)
    `(let ((p ,path)
	   (found nil))
       (while (and p (not found))
	 (let ((f (expand-file-name ,file (car p))))
	   (if (file-exists-p f)
	       (setq found f)))
	 (setq p (cdr p)))
       found)))

(defun company-async-semantic--find-dep (include)
  (when-let ((default-dir (expand-file-name default-directory))
	     (path (append (list default-dir) async-semantic-default-path))
	     (file (semantic-tag-name include)))
    (company-async-semantic--find-file-on-path file path)))

(defun company-async-semantic--get-includes-files-dep (file)
  (when-let* ((tags (assoc-default file company-async-semantic--cache))
	      (includes (seq-filter (lambda (tag)
				      (eq (semantic-tag-class tag) 'include))
				    tags)))
    (mapc (lambda (include)
	    (unless (member include company-async-semantic--files-dep)
	      (add-to-list 'company-async-semantic--files-dep include t)
	      (company-async-semantic--get-includes-files-dep include)))
	  (mapcar #'company-async-semantic--find-dep includes))))

(defun company-async-semantic--get-files-dep ()
  (let ((file (file-truename (buffer-file-name))))
    (setq company-async-semantic--files-dep (list file))
    (company-async-semantic--get-includes-files-dep file)))

(defun company-async-semantic--get-tags (files)
  (let (tags)
    (mapc (lambda (file)
	    (when-let ((tag (assoc-default file company-async-semantic--cache)))
	      (setq tags (append tags tag))))
	  files)
    tags))

(defun company-async-semantic--completions-system (prefix)
  (company-async-semantic--get-files-dep)
  (delq nil (mapcar (lambda (tag)
		      (let ((func (car tag)))
			(when (and (eq (semantic-tag-class tag) 'function)
				   (string-prefix-p prefix func))
			  func)))
		    (company-async-semantic--get-tags company-async-semantic--files-dep))))

(defun company-async-semantic--completions-scope (prefix)
  )

(defun company-async-semantic--completions-local (prefix)
  )

(defun company-async-semantic--completions (prefix)
  (delq nil (append (company-async-semantic--completions-local prefix)
		    (company-async-semantic--completions-scope prefix)
		    (company-async-semantic--completions-system prefix))))

(defun company-async-semantic--completions-raw (prefix)
  nil)

(defun company-async-semantic--update-cache (file)
  (when-let* ((filename (file-name-nondirectory file))
	      (dir (file-name-directory file))
	      (cache-file (semanticdb-cache-filename semanticdb-new-database-class dir))
	      (db (semanticdb-load-database cache-file))
	      (table (seq-find (lambda (table)
				 (string= filename (oref table file)))
			       (oref db tables)))
	      (tags (oref table tags)))
    (if (assoc file company-async-semantic--cache)
	(setcdr (assoc file company-async-semantic--cache) tags)
      (add-to-list 'company-async-semantic--cache (cons file tags)))))

(defun company-async-semantic--parse-done (files)
  (mapc #'company-async-semantic--update-cache files))

(defun company-async-semantic--need-parse ()
  (not (assoc (file-truename (buffer-file-name))
	      company-async-semantic--cache)))

(defun company-async-semantic--completion-raw-p (arg)
  (and (equal arg "") (not (looking-back "->\\|\\.\\|::" (- (point) 2)))))

(defun company-async-semantic--candidates (arg)
  (let (candidates)
    (if (company-async-semantic--need-parse)
	(async-semantic-buffer #'company-async-semantic--parse-done t)
      (setq candidates (if (company-async-semantic--completion-raw-p arg)
			   (company-async-semantic--completions-raw arg)
			 (company-async-semantic--completions arg))))
    candidates))

(defun company-async-semantic--prefix ()
  (and company-async-semantic-enabled
       (memq major-mode company-async-semantic-modes)
       (not (company-in-string-or-comment))
       (async-semantic-idle)
       (or (company-grab-symbol-cons "\\.\\|->\\|::" 2) 'stop)))

(defun company-async-semantic--after-save ()
  (when (memq major-mode company-async-semantic-modes)
    (if (company-async-semantic--need-parse)
	(async-semantic-buffer #'company-async-semantic--parse-done t)
      (async-semantic-buffer #'company-async-semantic--parse-done))))

;;; External Functions

(defun company-async-semantic-clear-cache ()
  (interactive)
  (setq company-async-semantic--cache nil))

(defun company-async-semantic-setup ()
  (if company-async-semantic-enabled
      (add-hook 'after-save-hook 'company-async-semantic--after-save)
    (remove-hook 'after-save-hook 'company-async-semantic--after-save)))

(defun company-async-semantic-toggle ()
  (interactive)
  (setq company-async-semantic-enabled (not company-async-semantic-enabled))
  (message (concat "Company Async Semantic: " (if company-async-semantic-enabled
						  (propertize "Enabled" 'face 'success)
						(propertize "Disabled" 'face 'error))))
  (company-async-semantic-setup))

(defun company-async-semantic (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-async-semantic))
    (prefix (company-async-semantic--prefix))
    (candidates (company-async-semantic--candidates arg))
    (duplicates t)))

(provide 'company-async-semantic)
