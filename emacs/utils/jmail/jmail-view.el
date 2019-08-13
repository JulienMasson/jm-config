;;; jmail-view.el --- XXXX

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

(require 'message)
(require 'jmail-attachment)
(require 'jmail-compose)

;;; Mode

(defvar jmail-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "R" 'jmail-view-reply)
    (define-key map "S" 'jmail-view-save-attachments)
    (define-key map "n" 'jmail-search-next)
    (define-key map "p" 'jmail-search-previous)
    (define-key map "q" 'jmail-view-quit)
    (define-key map "t" 'jmail-view-toggle-html)
    map)
  "Keymap for `jmail-view-mode'")

(define-derived-mode jmail-view-mode text-mode
  "jmail view"
  (toggle-read-only t))

;;; Faces

(defface jmail-view-0-face
  '((t :inherit font-lock-variable-name-face :bold nil :italic t))
  "Face for cited message parts (level 0)."
  :group 'jmail)

(defface jmail-view-1-face
  '((t :inherit font-lock-preprocessor-face :bold nil :italic t))
  "Face for cited message parts (level 1)."
  :group 'jmail)

(defface jmail-view-2-face
  '((t :inherit font-lock-constant-face :bold nil :italic t))
  "Face for cited message parts (level 2)."
  :group 'jmail)

(defface jmail-view-3-face
  '((t :inherit font-lock-function-name-face :bold nil :italic t))
  "Face for cited message parts (level 3)."
  :group 'jmail)

(defface jmail-view-4-face
  '((t :inherit font-lock-type-face :bold nil :italic t))
  "Face for cited message parts (level 4)."
  :group 'jmail)

(defface jmail-view-5-face
  '((t :inherit font-lock-comment-face :bold nil :italic t))
  "Face for cited message parts (level 5)."
  :group 'jmail)

(defface jmail-view-6-face
  '((t :inherit font-lock-comment-delimiter-face :bold nil :italic t))
  "Face for cited message parts (level 6)."
  :group 'jmail)

;;; Internal Variables

(defconst jmail-view--buffer-name "*jmail-view*")

(defconst jmail-view--cited-regexp
  "^\\(\\([[:alpha:]]+\\)\\|\\( *\\)\\)\\(\\(>+ ?\\)+\\)")

(defconst jmail-view--prompt-all-str "all")

(defvar-local jmail-view--data nil)

(defvar-local jmail-view--html-view nil)

;;; Internal Functions

(defmacro with-jmail-view-buffer (&rest body)
  `(when (get-buffer jmail-view--buffer-name)
     (with-current-buffer jmail-view--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-view--setup-buffer (buffer)
  (with-current-buffer (get-buffer-create jmail-view--buffer-name)
    (jmail-view-mode))
  (select-window (jmail-split-window-below buffer))
  (switch-to-buffer jmail-view--buffer-name))

(defun jmail-view--eoh-point ()
  (save-excursion
    (rfc822-goto-eoh)
    (point)))

(defun jmail-view--header-face-line ()
  (let* ((beg (line-beginning-position))
	 (end (line-end-position))
	 (str (buffer-substring beg end)))
    (cond
     ;; match To field
     ((string-match "^[Tt]o:" str)
      (add-face-text-property beg (+ beg (match-end 0))
			      'message-header-name)
      (add-face-text-property (+ beg (match-end 0)) end
			      'message-header-to))
     ;; match Cc or Reply field
     ((string-match "^\\(Cc\\|[Rr]eply-[Tt]o\\):" str)
      (add-face-text-property beg (+ beg (match-end 0))
			      'message-header-name)
      (add-face-text-property (+ beg (match-end 0)) end
			      'message-header-cc))
     ;; match Subject field
     ((string-match "^[Ss]ubject:" str)
      (add-face-text-property beg (+ beg (match-end 0))
			      'message-header-name)
      (add-face-text-property (+ beg (match-end 0)) end
			      'message-header-subject))
     ;; match other
     ((string-match "^[A-Z][^: \n\t]+:" str)
      (add-face-text-property beg (+ beg (match-end 0))
			      'message-header-name)
      (add-face-text-property (+ beg (match-end 0)) end
			      'message-header-other)))))

(defun jmail-view--set-header-faces ()
  (save-excursion
    (goto-char (point-min))
    (let ((end (jmail-view--eoh-point)))
      (while (< (point) end)
	(jmail-view--header-face-line)
	(forward-line)))))

(defun jmail-view--clean-body ()
  (save-excursion
    (message-goto-body)
    (while (re-search-forward "
      (replace-match ""))))

(defun jmail-view--fontify-cited ()
  (save-excursion
    (message-goto-body)
    (while (re-search-forward jmail-view--cited-regexp nil t)
      (when-let* ((str (buffer-substring (line-beginning-position) (point)))
		  (level (mod (string-width (replace-regexp-in-string
					     "[^>]" "" str)) 7))
		  (cited-face (intern-soft (format "jmail-view-%d-face" level))))
	(add-face-text-property (line-beginning-position)
				(line-end-position)
				cited-face)))))

(defun jmail-view--minimal-diff-face ()
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward "^diff \-\-git" nil t))
    (while (not (eobp))
      (let* ((start (point))
	     (end (line-end-position))
	     (str (buffer-substring-no-properties start end))
	     (inhibit-read-only t))
	(cond ((string-match "^\\(---\\|\\+\\+\\+\\)" str)
	       (add-face-text-property start end 'diff-file-header))
	      ((string-match "^@@" str)
	       (add-face-text-property start end 'diff-header))
	      ((string-match "^\\+" str)
	       (add-face-text-property start end 'diff-added))
	      ((string-match "^\\-" str)
	       (add-face-text-property start end 'diff-removed)))
	(forward-line)))))

(defun jmail-view--insert-html (html)
  (let ((beg (point)))
    (insert html)
    (shr-render-region beg (point))))

(defun jmail-view--address-str (field)
  (when-let ((data (plist-get jmail-view--data field)))
    (mapconcat #'jmail-make-address-str data ", ")))

(defun jmail-view--date-str ()
  (when-let ((date (plist-get jmail-view--data :date)))
    (format-time-string "%a, %e %b %Y %T %z" date)))

(defun jmail-view--get-attachments ()
  (when-let* ((parts (plist-get jmail-view--data :parts))
	      (attachments (seq-filter (lambda (elem)
					 (plist-get elem :attachment))
				       parts)))
    (mapcar (lambda (elem)
	      (cons (plist-get elem :name) (plist-get elem :index)))
	    attachments)))

(defun jmail-view--attachments-prompt ()
  (when-let ((attachments (jmail-view--get-attachments)))
    (append (list jmail-view--prompt-all-str)
	    (mapcar #'car attachments))))

(defun jmail-view--attachments-str ()
  (when-let ((attachments (jmail-view--get-attachments)))
    (mapconcat #'car attachments ", ")))

(defun jmail-view--insert-contents ()
  (let ((from (jmail-view--address-str :from))
	(to (jmail-view--address-str :to))
	(cc (jmail-view--address-str :cc))
	(mailing-list (plist-get jmail-view--data :mailing-list))
	(subject (plist-get jmail-view--data :subject))
	(date (jmail-view--date-str))
	(attachments (jmail-view--attachments-str))
	(plain-text (plist-get jmail-view--data :body-txt))
	(html (plist-get jmail-view--data :body-html)))
    (goto-char (point-min))
    (cl-macrolet ((insert-header (field)
		   `(when ,field
		      (message-insert-header ',field ,field)
		      (insert "\n"))))
      (insert-header from)
      (insert-header to)
      (insert-header cc)
      (insert-header mailing-list)
      (insert-header subject)
      (insert-header date)
      (insert-header attachments))
    (if jmail-view--html-view
	(if html (jmail-view--insert-html html))
      (if plain-text (insert plain-text)))))

(defun jmail-view--insert-mail ()
  (when jmail-view--data
    (with-jmail-view-buffer
     (erase-buffer)
     (jmail-view--insert-contents)
     (jmail-view--set-header-faces)
     (unless jmail-view--html-view
       (jmail-view--clean-body)
       (jmail-view--fontify-cited)
       (jmail-view--minimal-diff-face))
     (set-buffer-modified-p nil)
     (goto-char (point-min)))))

(defun jmail-view--process-sentinel (process status)
  (when (eq (process-exit-status process) 0)
    (let* ((buffer (process-buffer process))
	   (object (jmail-extract-sexp-object buffer)))
      (kill-buffer buffer)
      (with-jmail-view-buffer
       (setq jmail-view--data object)
       (jmail-view--insert-mail)))))

(defun jmail-view--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

(defun jmail-view--get-mail-data (path)
  (when-let* ((program (jmail-find-program jmail-index-program))
	      (args (list "view" "--nocolor" "--format=sexp" path))
	      (buffer (get-buffer-create "*jmail-view-process*"))
	      (process (apply 'start-file-process "jmail-view" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-view--process-filter)
    (set-process-sentinel process 'jmail-view--process-sentinel)))

(defun jmail-view--insert-reply-text (from plain-text)
  (save-excursion
    (message-goto-body)
    (insert "\n")
    (message-insert-formatted-citation-line from (message-make-date))
    (let ((beg (point)))
      (insert plain-text)
      (jmail-view--clean-body)
      (message-indent-citation beg (point)))))

(defun jmail-view--autodetect-account ()
  (when-let* ((accounts (jmail-get-accounts jmail-smtp-config-file))
	      (accounts-address (mapcar #'cddr accounts))
	      (to (plist-get jmail-view--data :to))
	      (to-address (mapcar #'cdr to))
	      (address (car (cl-intersection accounts-address to-address
					     :test #'string=))))
    (seq-find (lambda (elem)
		(string= address (cddr elem)))
	      accounts)))

(defun jmail-view--reply-get-to (from)
  (when-let ((to (append (plist-get jmail-view--data :from)
			 (plist-get jmail-view--data :to)))
	     (to-list (mapcar #'jmail-make-address-str to)))
    (mapconcat 'identity (seq-remove (lambda (elem)
				       (string= from elem))
				     to-list) ", ")))

;;; External Functions

(defun jmail-view-reply ()
  (interactive)
  (with-jmail-view-buffer
   (let* ((account (jmail-view--autodetect-account))
	  (from (jmail-make-address-str (cdr account)))
	  (from-email (cddr account))
	  (to (jmail-view--reply-get-to from))
	  (cc (jmail-view--address-str :cc))
	  (subject (plist-get jmail-view--data :subject))
	  (plain-text (plist-get jmail-view--data :body-txt))
	  (in-reply-to (plist-get jmail-view--data :in-reply-to)))
     (message-pop-to-buffer (message-buffer-name "reply" to))
     (jmail-compose-mode)
     (jmail-compose-set-extra-arguments (car account) from-email)
     (message-setup `((From . ,from)
		      (To . ,to)
		      (Cc . ,cc)
		      (Subject . ,(message-simplify-subject subject))
		      (In-reply-to . ,in-reply-to)))
     (message-sort-headers)
     (message-hide-headers)
     (when plain-text
       (jmail-view--insert-reply-text from plain-text))
     (message-goto-body))))

(defun jmail-view-save-attachments (attachments outdir)
  (interactive (list (completing-read "Save: "
				      (jmail-view--attachments-prompt))
		     (read-directory-name "Path: ")))
  (when-let ((msg-path (plist-get jmail-view--data :path)))
    (if (string= attachments jmail-view--prompt-all-str)
	(jmail-attachment-save-all msg-path outdir)
      (jmail-attachment-save msg-path
			     (assoc-default attachments
					    (jmail-view--get-attachments))
			     outdir))))

(defun jmail-view-toggle-html ()
  (interactive)
  (with-jmail-view-buffer
   (setq jmail-view--html-view (not jmail-view--html-view))
   (jmail-view--insert-mail)))

(defun jmail-view-quit ()
  (interactive)
  (when (get-buffer jmail-view--buffer-name)
    (with-jmail-view-buffer
     (kill-buffer-and-window))))

(defun jmail-view (path buffer)
  (if (get-buffer jmail-view--buffer-name)
      (switch-to-buffer-other-window jmail-view--buffer-name)
    (jmail-view--setup-buffer buffer))
  (jmail-view--get-mail-data path))

(provide 'jmail-view)