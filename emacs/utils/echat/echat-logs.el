;;; echat-logs.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2020-04-06

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

;;; Customization

(defcustom echat-logs-directory "~/.cache/echat-logs"
  "The directory where log files are stored."
  :type 'directory
  :group 'echat)

;;; Internal Functions

(defun echat-logs--filename (echat)
  (let ((type (object-class-name echat))
	(name (oref echat name))
	(buffer (buffer-name (current-buffer))))
    (format "%s-%s-%s.txt" type name
	    (replace-regexp-in-string "/" "!" buffer))))

(defun echat-logs--load (echat)
  (let ((default-directory echat-logs-directory)
	(filename (echat-logs--filename echat))
	(regexp "^\\(\([0-9]+ [0-9]+ [0-9]+ [0-9]+\)\\);\\(\[^;\]*\\);\\(.*\\)")
	data)
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(add-to-list 'data (list (read (match-string 1))
				 (match-string 2)
				 (match-string 3)) t))
      (kill-current-buffer))
    data))

(defun echat-logs--insert (button)
  (when-let ((echat (button-get button 'echat-logs-echat))
	     (me (button-get button 'echat-logs-me))
	     (data (button-get button 'echat-logs-data))
	     (beg (line-beginning-position))
	     (end (+ (line-end-position) 1))
	     (inhibit-read-only t)
	     (last-pos t))
    (save-excursion
      (echat-ui--remove-separator)
      (delete-region beg end)
      (setq last-pos (marker-position lui-output-marker))
      (set-marker lui-output-marker beg)
      (pcase-dolist (`(,time ,sender ,body) data)
	(echat-ui--insert-msg echat sender me body time nil))
      (set-marker lui-output-marker (+ (marker-position lui-output-marker)
				       (- last-pos 1)))
      (echat-ui--insert-separator))))

;;; External Functions

(defun echat-logs-insert-load-more (echat me)
  (when-let ((data (echat-logs--load echat)))
    (insert-text-button "(load more)" 'action #'echat-logs--insert
			'echat-logs-echat echat
			'echat-logs-me me
			'echat-logs-data data)
    (insert "\n")))

(defun echat-logs-save (echat sender body time)
  (unless (file-directory-p echat-logs-directory)
    (make-directory echat-logs-directory t))
  (let ((default-directory echat-logs-directory)
	(filename (echat-logs--filename echat)))
    (with-temp-buffer
      (insert (format "%s;%s;%s\n" time sender body))
      (write-region nil nil filename t 'nomessage))))

(provide 'echat-logs)
