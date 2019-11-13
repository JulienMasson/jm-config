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
(require 'company-semantic)
(require 'semantic/db-mode)

;;; Customization

(defcustom company-jm-modes '(c-mode c++-mode)
  "List of mode when `company-jm' is enabled"
  :type 'list
  :group 'company)

(defcustom company-jm-enabled nil
  "Enable `company-jm', by default it's disable"
  :type 'boolean
  :group 'company)

;;; Internal Functions

(defun company-jm--setup-buffer ()
  (semantic-default-c-setup)
  (setq semantic-new-buffer-fcn-was-run t)
  (semantic-lex-init)
  (semantic-clear-toplevel-cache)
  (semanticdb-semantic-init-hook-fcn))

(defun company-jm--buffer-parsed ()
  (let* ((dd (file-name-directory (file-truename (buffer-file-name))))
	 (cache-file (semanticdb-cache-filename semanticdb-new-database-class dd)))
    (file-exists-p cache-file)))

(defun company-jm--candidates (arg)
  (let (candidates)
    (if (not (company-jm--buffer-parsed))
	(async-semantic-db-buffer)
      (unless (semantic-active-p)
	(company-jm--setup-buffer)
	(semantic-fetch-tags))
      (when (semantic-fetch-available-tags)
	(setq candidates (if (and (equal arg "")
				  (not (looking-back "->\\|\\.\\|::" (- (point) 2))))
			     (company-semantic-completions-raw arg)
			   (company-semantic-completions arg)))))
    candidates))

(defun company-jm--prefix ()
  (and company-jm-enabled
       (memq major-mode company-jm-modes)
       (not (company-in-string-or-comment))
       (or (company-semantic--prefix) 'stop)))

;;; External Functions

(defun company-jm-toggle ()
  (interactive)
  (setq company-jm-enabled (not company-jm-enabled))
  (if company-jm-enabled
      (progn
	;; add semanticdb hooks
	(dolist (elt semanticdb-hooks)
	  (add-hook (cadr elt) (car elt)))
	(message (concat "Company JM: "
			 (propertize "Enabled" 'face 'success))))
    ;; remove semanticdb hooks
    (dolist (elt semanticdb-hooks)
      (remove-hook (cadr elt) (car elt)))
    (message (concat "Company JM: "
		     (propertize "Disabled" 'face 'error)))))

(defun company-jm (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-jm))
    (prefix (company-jm--prefix))
    (candidates (company-jm--candidates arg))
    (duplicates t)))

(provide 'company-jm)
