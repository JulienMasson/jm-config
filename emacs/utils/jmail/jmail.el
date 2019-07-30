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

(require 'subr-x)
(require 'jmail-count)
(require 'jmail-search)
(require 'jmail-update)
(require 'jmail-utils)

(defgroup jmail nil
  "Mail reader for Emacs."
  :group 'mail)

;;; Mode

(defvar jmail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "U"      'jmail-unread-all)
    (define-key map "g"      'jmail-update-buffer)
    (define-key map "j"      'jmail-jump-to-maildir)
    (define-key map "n"      'jmail-next-query)
    (define-key map "p"      'jmail-previous-query)
    (define-key map "q"      'jmail-quit)
    (define-key map "u"      'jmail-unread)
    (define-key map [down]   'jmail-next-query)
    (define-key map [return] 'jmail-enter)
    (define-key map [up]     'jmail-previous-query)
    map)
  "Keymap for `jmail-mode'")

(define-derived-mode jmail-mode fundamental-mode
  "jmail"
  (jmail--update-header-line jmail--default-header)
  (jmail--insert-header)
  (jmail--insert-queries)
  (toggle-read-only t))

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
  :type 'string
  :group 'jmail)

(defcustom jmail-queries nil
  "Queries displayed in menu"
  :group 'jmail)

(defcustom jmail-update-buffer-every nil
  "Update buffer every X seconds, if nil don't refresh"
  :type 'integer
  :group 'jmail)

(defcustom jmail-unread-count-hook nil
  "Functions called when counting unread message"
  :type 'hook
  :group 'jmail)

(defcustom jmail-sync-config-file nil
  "Path to the config file used by `jmail-sync-program'"
  :type 'string
  :group 'jmail)

;;; External Variables

(defconst jmail-index-program "mu")

(defconst jmail-sync-program "mbsync")

;;; Internal Variables

(defconst jmail--buffer-name "*jmail*")

(defconst jmail--unknown-count "(/)")

(defconst jmail--default-header "       Welcome to Jmail !")

(defconst jmail--update-ongoing (propertize 	"       Update ongoing ..."
						'face 'warning))

(defconst jmail--update-error (propertize 	"       Update Failed !"
						'face 'error))

(defvar jmail--update-timer nil)

;;; Internal Functions

(defun jmail--insert-header ()
  (insert "\n")
  (insert (propertize "  Queries\n" 'face 'jmail-queries-face))
  (insert "\n"))

(defun jmail--insert-query (query)
  (insert (propertize (format "%-5s %-18s" " " query) 'face 'bold)))

(defun jmail--insert-queries ()
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
	jmail-queries))

(defmacro with-jmail-buffer (&rest body)
  `(when (get-buffer jmail--buffer-name)
     (with-current-buffer jmail--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail--update-header-line (str)
  (with-jmail-buffer
   (setq header-line-format str)
   (force-mode-line-update)))

(defun jmail--setup ()
  (with-current-buffer (get-buffer-create jmail--buffer-name)
    (jmail-mode))
  (when jmail-update-buffer-every
    (jmail--restart-update-timer))
  (jmail--goto-first-query))

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
     (run-hook-with-args 'jmail-unread-count-hook
     			 (get-text-property (point) 'jmail)
     			 count)
     (end-of-line)
     (when (re-search-backward "(.*/\\(.*\\))$" nil t)
       (let ((beg (point)))
	 (replace-match (format "(%d/\\1)" count))
	 (if (> count 0)
	     (jmail-bold-region beg (point))
	   (jmail-unbold-region beg (point))))))))

(defun jmail--get-counts ()
  (jmail--foreach-query line query
    (jmail-count-get query #'jmail--count-total-handler line)
    (jmail-count-get (concat query " flag:unread")
    		     #'jmail--count-unread-handler line)))

(defun jmail--update-buffer-success ()
  (jmail--update-header-line jmail--default-header)
  (jmail--get-counts))

(defun jmail--update-buffer-error ()
  (jmail--update-header-line jmail--update-error))

(defun jmail--start-update-buffer ()
  (jmail--update-header-line jmail--update-ongoing)
  (jmail-update #'jmail--update-buffer-success
		#'jmail--update-buffer-error))

(defun jmail--stop-update-timer ()
  (when jmail--update-timer
    (cancel-timer jmail--update-timer)))

(defun jmail--start-update-timer ()
  (setq jmail--update-timer (run-at-time 1 jmail-update-buffer-every
					 'jmail--start-update-buffer)))

(defun jmail--restart-update-timer ()
   (jmail--stop-update-timer)
   (jmail--start-update-timer))

(defun jmail--maildir-name-list ()
  (let (maildir)
    (jmail--foreach-query line query
      (when (string-match "maildir:/\\(.*\\)" query)
	(add-to-list 'maildir (match-string 1 query))))
    maildir))

(defun jmail--global-check ()
  (if jmail-top-maildir
      (unless (file-exists-p jmail-top-maildir)
	(jmail-abort (concat "Cannot find: " jmail-top-maildir)))
    (jmail-abort "Please set `jmail-top-maildir'"))
  (unless jmail-queries
    (jmail-abort "Please set `jmail-queries'"))
  (unless (jmail-find-program-from-top jmail-index-program)
    (jmail-abort (concat "Please install " jmail-index-program)))
  (unless (jmail-find-program-from-top jmail-sync-program)
    (jmail-abort (concat "Please install " jmail-sync-program))))

;;; External Functions

(defun jmail-previous-query ()
  (interactive)
  (jmail--move-to-query nil))

(defun jmail-next-query ()
  (interactive)
  (jmail--move-to-query t))

(defun jmail-update-buffer ()
  (interactive)
  (if jmail-update-buffer-every
      (jmail--restart-update-timer)
    (jmail--start-update-buffer)))

(defun jmail-quit ()
  (interactive)
  (jmail-search-quit)
  (with-jmail-buffer
   (jmail--stop-update-timer)
   (kill-this-buffer)))

(defun jmail-enter ()
  (interactive)
  (if-let ((prop (get-text-property (point) 'jmail)))
      (jmail-search prop t nil)))

(defun jmail-jump-to-maildir (query)
  (interactive (list (completing-read "Jump to: "
				      (jmail--maildir-name-list))))
  (jmail-search (concat "maildir:/" query) t nil))

(defun jmail-unread-all ()
  (interactive)
  (jmail-search "flag:unread" t nil))

(defun jmail-unread ()
  (interactive)
  (if-let* ((prop (get-text-property (point) 'jmail))
	    (unread (concat prop " flag:unread")))
      (jmail-search unread t nil)))

(defun jmail ()
  (interactive)
  (jmail--global-check)
  (unless (get-buffer jmail--buffer-name)
    (jmail--setup))
  (jmail--get-counts)
  (jmail-switch-to-buffer jmail--buffer-name))

(provide 'jmail)
