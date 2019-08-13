;;; jmail-company.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-07-12

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'company)

;;; Internal Variables

(defconst jmail-company--address-fields-regexp
  "^\\(To\\|B?Cc\\|Reply-To\\|From\\):.*")

;;; Internal Functions

(defun jmail-company--candidate-at-line ()
  (when-let ((str (buffer-substring (line-beginning-position)
				    (line-end-position)))
	     (mail-regexp "\\([[:graph:]]*\\)@\\([[:graph:]]*\\)$"))
    (cond ((string-match (concat "\\(.*\\) " mail-regexp) str)
	   (format "%s <%s@%s>" (match-string 1 str)
		   (match-string 2 str)
		   (match-string 3 str)))
	  ((string-match mail-regexp str)
	   (format "<%s@%s>" (match-string 1 str)
		   (match-string 2 str))))))

(defun jmail-company--candidates (arg)
  (let ((args (list "cfind" "--nocolor" arg))
	candidates)
    (with-temp-buffer
      (apply #'process-file "mu" nil (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
	(add-to-list 'candidates (jmail-company--candidate-at-line))
	(forward-line)))
    candidates))

;;; External Functions

(defun jmail-company (command &optional arg &rest _ignore)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'jmail-company))
    (prefix (and (derived-mode-p 'message-mode)
		 (looking-back jmail-company--address-fields-regexp
			       (line-beginning-position))
		 (company-grab "[:,][ \t]*\\(.*\\)" 1
			       (line-beginning-position))))
    (candidates (jmail-company--candidates arg))
    (no-cache t)))

(defun jmail-company-setup ()
  (company-mode)
  (setq-local company-backends '(jmail-company)))

(provide 'jmail-company)
