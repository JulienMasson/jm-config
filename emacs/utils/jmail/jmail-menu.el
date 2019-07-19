;;; jmail-menu.el --- XXXX

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

(require 'jmail-search)
(require 'jmail-update)

(define-derived-mode jmail-menu-mode fundamental-mode
  "jmail menu"
  (toggle-read-only t))

(define-key jmail-menu-mode-map [return] 'jmail-menu-enter)
(define-key jmail-menu-mode-map "g" 'jmail-menu-refresh)
(define-key jmail-menu-mode-map "n" 'jmail-menu-next)
(define-key jmail-menu-mode-map [down] 'jmail-menu-next)
(define-key jmail-menu-mode-map "p" 'jmail-menu-previous)
(define-key jmail-menu-mode-map [up] 'jmail-menu-previous)
(define-key jmail-menu-mode-map "q" 'jmail-menu-quit)
(define-key jmail-menu-mode-map "s" 'jmail-menu-search)
(define-key jmail-menu-mode-map "u" 'jmail-menu-unread)
(define-key jmail-menu-mode-map "U" 'jmail-menu-unread-all)

;;; External Variables

(defvar jmail-menu-queries nil)

;;; Internal Variables

(defconst jmail-menu--buffer-name "*jmail-menu*")

;;; Internal Functions

(defmacro with-jmail-menu-buffer (&rest body)
  `(let ((inhibit-read-only t))
     (if (get-buffer jmail-menu--buffer-name)
	 (with-current-buffer jmail-menu--buffer-name
	          ,@body)
       (with-current-buffer (get-buffer-create jmail-menu--buffer-name)
	 (jmail-menu-mode)
	 ,@body))))

(defun jmail-menu--get-count (query)
  (let ((cmd (format "mu find %s 2>/dev/null | wc -l" query)))
    (string-to-number (string-trim (shell-command-to-string cmd)))))

(jmail-menu--get-count "maildir:/Baylibre/Kernel flag:unread")

(defun jmail-menu--insert-queries ()
  (with-jmail-menu-buffer
   (mapc (lambda (data)
	   (let ((header (car data))
		 (queries (cdr data)))
	     (when header
	       (insert (propertize (format "    * %s \n" (upcase header))
				   'face 'font-lock-variable-name-face)))
	     (mapc (lambda (query)
		     (let* ((beg (point))
			    (cmd-total (cdr query))
			    (total (jmail-menu--get-count cmd-total))
			    (cmd-unread (concat cmd-total " flag:unread"))
			    (unread (jmail-menu--get-count cmd-unread)))
		       (insert (propertize
				(format "%-5s %-18s" " " (car query))
			       'face 'bold))
		       (insert (propertize
  				(format "(%d/%d)" unread total)
  				'face (if (> unread 0)
  					  'notmuch-unread-count-face
  					'notmuch-total-count-face)))
		       (put-text-property beg (point) 'jmail (cdr query))
		       (insert "\n")))
		   queries)
	     (insert "\n")))
	 jmail-menu-queries)))

(defun jmail-menu--setup-buffer ()
  (with-jmail-menu-buffer
   (erase-buffer)
   (insert "\n")
   (insert (propertize "  Queries\n" 'face 'notmuch-title-face))
   (insert "\n")
   (jmail-menu--insert-queries)
   (goto-char (point-min))
   (jmail-menu-next)))

;;; External Functions

(defun jmail-menu-previous ()
  (interactive)
  (beginning-of-line)
  (re-search-backward "[[:graph:]]" nil t)
  (beginning-of-line)
  (re-search-forward "[[:graph:]]" nil t)
  (backward-char))

(defun jmail-menu-next ()
  (interactive)
  (end-of-line)
  (re-search-forward "[[:graph:]]" nil t)
  (backward-char))

(defun jmail-menu-enter ()
  (interactive)
  (if-let ((prop (get-text-property (point) 'jmail)))
      (jmail-search prop t nil)))

(defun jmail-menu-unread-all ()
  (interactive)
  (jmail-search "flag:unread" t nil))

(defun jmail-menu-unread ()
  (interactive)
  (if-let* ((prop (get-text-property (point) 'jmail))
	    (unread (concat prop " flag:unread")))
      (jmail-search unread t nil)))

(defun jmail-menu-quit ()
  (interactive)
  (with-jmail-menu-buffer
   (kill-current-buffer)))

(defun jmail-menu-refresh ()
  (interactive)
  (jmail-update #'jmail-menu--setup-buffer))

(defun jmail-menu-search (search)
  (interactive "sSearch: ")
  (jmail-search search t nil))

(defun jmail-menu ()
  (interactive)
  (jmail-menu--setup-buffer)
  (switch-to-buffer jmail-menu--buffer-name))

(provide 'jmail-menu)
