;;; jmail-utils.el --- XXXX

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

;;; External Functions

(defun jmail-abort (msg)
  (error (substring-no-properties msg)))

(defun jmail-funcall (func &rest args)
  (condition-case-unless-debug err
      (apply func args)
    (error (message "Error %s: %S" (symbol-name func) err))))

(defun jmail-switch-to-buffer (buffer)
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

(defun jmail-tramp-executable-find (dir program-name)
  (with-parsed-tramp-file-name dir nil
    (let ((buffer (tramp-get-connection-buffer v))
	  (cmd (concat "which " program-name)))
      (with-current-buffer buffer
	(tramp-send-command v cmd)
	(goto-char (point-min))
	(when (looking-at "^\\(.+\\)")
	  (match-string 1))))))

(defun jmail-find-program (dir program-name)
  (let ((default-directory dir))
    (if (tramp-tramp-file-p dir)
	(jmail-tramp-executable-find dir program-name)
      (executable-find program-name))))

(defun jmail-find-visible-character (from forward)
  (save-excursion
    (goto-char from)
    (if forward
	(progn
	  (end-of-line)
	  (when (re-search-forward "[[:graph:]]" nil t)
	    (- (point) 1)))
      (beginning-of-line)
      (when (re-search-backward "[[:graph:]]" nil t)
	(beginning-of-line)
	(re-search-forward "[[:graph:]]" nil t)
	(- (point) 1)))))

(defun jmail-autofill-maildir-queries (top)
  (when (file-exists-p top)
    (let* ((path (expand-file-name top))
	   (dirs (directory-files-recursively path "cur$" t))
  	   (subdirs (mapcar (lambda (dir)
  			      (replace-regexp-in-string
  			       (format "%s/\\(.*\\)/cur" path)
  			       "\\1" dir)) dirs))
	   queries)
      (mapc (lambda (elem)
	      (let ((cmd (format "maildir:/%s" elem)))
		(cl-multiple-value-bind (top sub)
		    (split-string elem "/")
		  (if (assoc top queries)
		      (let ((data (assoc-default top queries)))
		      	(setcdr (assoc top queries)
		      		(add-to-list 'data `(,sub . ,cmd) t)))
		    (add-to-list 'queries (cons top `((,sub . ,cmd))) t)))))
	    subdirs)
      queries)))

(provide 'jmail-utils)
