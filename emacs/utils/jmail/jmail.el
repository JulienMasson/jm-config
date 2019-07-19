;;; jmail.el --- XXXX

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

(require 'jmail-count)
(require 'jmail-utils)

(defgroup jmail nil
  "Mail reader for Emacs."
  :group 'mail)

(define-derived-mode jmail-mode fundamental-mode
  "jmail"
  (setq header-line-format "       Welcome to Jmail !")
  (toggle-read-only t))

(defvar jmail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"    'jmail-next-query)
    (define-key map [down] 'jmail-next-query)
    (define-key map "p"    'jmail-previous-query)
    (define-key map [up]   'jmail-previous-query)
    map)
  "Keymap for `jmail-mode'")

;;; Faces

(defface jmail-queries-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for a Queries header"
  :group 'jmail)

(defface jmail-account-face
  '((t :inherit font-lock-variable-name-face))
  "Face for a account"
  :group 'jmail)

;;; Customization

(defcustom jmail-top-maildir nil
  "Path to the top of the maildir"
  :group 'jmail)

(defcustom jmail-queries nil
  "Queries displayed in menu"
  :group 'jmail)

;;; Internal Variables

(defconst jmail--buffer-name "*jmail*")

(defconst jmail--unknown-count "(/)")

;;; Internal Functions

(defmacro with-jmail-buffer (&rest body)
  `(let ((inhibit-read-only t))
     (with-current-buffer jmail--buffer-name
       ,@body)))

(defun jmail--global-check ()
  (if jmail-top-maildir
      (unless (file-exists-p jmail-top-maildir)
	(jmail-abort (concat "Cannot find: " jmail-top-maildir)))
    (jmail-abort "Please set `jmail-top-maildir'"))
  (unless jmail-queries
    (jmail-abort "Please set `jmail-queries'"))
  ;; TODO: check all progams found: mu and mbsync
  )

(defun jmail--setup-buffer ()
  (unless (get-buffer jmail--buffer-name)
    (with-current-buffer (get-buffer-create jmail--buffer-name)
      (jmail-mode))))

(defun jmail--insert-header ()
  (with-jmail-buffer
   (erase-buffer)
   (insert "\n")
   (insert (propertize "  Queries\n" 'face 'jmail-queries-face))
   (insert "\n")))

(defun jmail--insert-query (query)
  (insert (propertize (format "%-5s %-18s" " " query) 'face 'bold)))

(defun jmail--insert-queries ()
  (with-jmail-buffer
   (mapc (lambda (data)
	   (let ((account (car data))
		 (queries (cdr data)))
	     (when account
	       (insert (propertize (format "    * %s \n" (upcase account))
				   'face 'jmail-account-face)))
	     (mapc (lambda (query)
		     (let ((beg (point)))
		       (jmail--insert-query (car query))
		       (insert jmail--unknown-count)
		       (put-text-property beg (point) 'jmail (cdr query))
		       (insert "\n")))
		   queries)
	     (insert "\n")))
	 jmail-queries)))

(defun jmail--move-to-query (forward)
  (with-jmail-buffer
   (let* ((pos (jmail-find-visible-character (point) forward))
	  (prop (if pos (get-text-property pos 'jmail))))
     (while (and pos (not prop))
       (save-excursion
	 (goto-char pos)
	 (setq pos (jmail-find-visible-character (point) forward))
	 (when pos
	   (setq prop (get-text-property pos 'jmail)))))
     (when (and pos prop)
       (goto-char pos)))))

(defun jmail--goto-first-query ()
  (with-jmail-buffer
   (goto-char (point-min))
   (jmail-next-query)))

(defun jmail--line-first-query ()
  (with-jmail-buffer
   (save-excursion
     (jmail--goto-first-query)
     (line-number-at-pos))))

(defun jmail--goto-last-query ()
  (with-jmail-buffer
   (goto-char (point-max))
   (jmail-previous-query)))

(defun jmail--line-last-query ()
  (with-jmail-buffer
   (save-excursion
     (jmail--goto-last-query)
     (line-number-at-pos))))

(defmacro jmail--foreach-query (line query &rest body)
  (declare (indent 2))
  `(with-jmail-buffer
    (save-excursion
      (when-let ((first (jmail--line-first-query))
		 (last (jmail--line-last-query)))
	(goto-line first)
	(setq ,line first)
	(setq ,query (get-text-property (point) 'jmail))
	,@body
	(while (not (= ,line last))
	  (jmail-next-query)
	  (setq ,line (line-number-at-pos))
	  (setq ,query (get-text-property (point) 'jmail))
	  ,@body)))))

(defun jmail--count-total-handler (count data)
  (with-jmail-buffer
   (save-excursion
     (goto-line data)
     (end-of-line)
     (when (re-search-backward "/.*)$" nil t)
       (replace-match (format "/%d)" count))))))

(defun jmail--count-unread-handler (count data)
  (with-jmail-buffer
   (save-excursion
     (goto-line data)
     (end-of-line)
     (when (re-search-backward "(.*/\\(.*\\))$" nil t)
       (replace-match (format "(%d/\\1)" count))))))

(defun jmail--get-counts ()
  (jmail--foreach-query line query
    (jmail-count-get query #'jmail--count-total-handler line)
    (jmail-count-get (concat query " flag:unread")
    		     #'jmail--count-unread-handler line)))

;;; External Functions

(defun jmail-previous-query ()
  (interactive)
  (jmail--move-to-query nil))

(defun jmail-next-query ()
  (interactive)
  (jmail--move-to-query t))

(defun jmail ()
  (interactive)
  (jmail--global-check)
  (jmail--setup-buffer)
  (jmail-switch-to-buffer jmail--buffer-name)
  (jmail--insert-header)
  (jmail--insert-queries)
  (jmail--get-counts)
  (jmail--goto-first-query))

(provide 'jmail)
