;;; jmail-nntp.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-10-21

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

(require 'nntp)

;;; Mode

(defvar jmail-nntp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'jmail-nntp-enter)
    (define-key map "g" 'jmail-nntp-refresh)
    (define-key map "q" 'jmail-nntp-quit)
    (define-key map "L" 'jmail-nntp-display-all)
    map)
  "Keymap for `jmail-nntp-mode'")

(define-derived-mode jmail-nntp-mode fundamental-mode
  "jmail nntp"
  (setq header-line-format (format jmail-search--format " 🔽" "Date" "From" "Subject"))
  (force-mode-line-update)
  (setq-local hl-line-face 'jmail-search-hl-line)
  (setq truncate-lines t)
  (add-hook 'window-scroll-functions #'jmail-nntp--after-scroll nil t)
  (toggle-read-only t))

;;; Customization

(defcustom jmail-nntp-server "nntp.lore.kernel.org"
  "NNTP server"
  :type 'string
  :group 'jmail)

(defcustom jmail-nntp-group "org.infradead.lists.linux-amlogic"
  "NNTP group"
  :type 'string
  :group 'jmail)

;;; Internal Variables

(defvar jmail-nntp--buffer-name "*jmail-nntp*")

(defvar jmail-nntp--process-buffer-name "*jmail-nntp-process*")

(defvar jmail-nntp--groups nil)

(defvar jmail-nntp--remainings-articles 0)

;;; Internal Functions

(defmacro with-jmail-nntp-buffer (&rest body)
  `(when (get-buffer jmail-nntp--buffer-name)
     (with-current-buffer jmail-nntp--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defmacro jmail-nntp--parse-results (cmd &rest body)
  `(with-current-buffer jmail-nntp--process-buffer-name
     (erase-buffer)
     (funcall ,cmd)
     (goto-char (point-min))
     (forward-line)
     (while (not (eobp))
	(let ((line (buffer-substring (line-beginning-position)
				      (line-end-position))))
	  ,@body
	  (forward-line)))))

(defun jmail-nntp--get-groups ()
  (let ((cmd (lambda () (nntp-request-list-newsgroups jmail-nntp-server)))
	(regexp "\\([[:graph:]]*\\) \\([[:graph:]]*\\) .*")
	groups)
    (jmail-nntp--parse-results cmd
      (when (string-match regexp line)
	(add-to-list 'groups (cons (match-string 1 line)
				   (match-string 2 line)) t)))
    groups))

(defun jmail-nntp--get-nbr-articles ()
  (with-current-buffer jmail-nntp--process-buffer-name
    (erase-buffer)
    (nntp-request-group-articles jmail-nntp-group)
    (goto-char (point-min))
    (let ((line (buffer-substring (line-beginning-position)
				  (line-end-position)))
	  (regexp (concat "[0-9]+ [0-9]+ [0-9]+ \\([0-9]+\\) "
			  jmail-nntp-group)))
      (when (string-match regexp line)
	(string-to-number (match-string 1 line))))))

(defun jmail-nntp--get-from (from)
  (let ((elem (gnus-extract-address-components from)))
    (if-let ((name (car elem)))
	name
      (cadr elem))))

(defun jmail-nntp--insert-header (line)
  (when (string-match "^\\([0-9]+\\)\t\\([[:print:]]+\\)\t\\([[:print:]]+\\)\t\\([[:print:]]+\\)" line)
    (let* ((article (string-to-number (match-string 1 line)))
	   (subject (match-string 2 line))
	   (from (match-string 3 line))
	   (date (match-string 4 line))
	   (from-str (jmail-nntp--get-from from))
	   (date-str (date-to-time date))
	   object)
      (with-jmail-nntp-buffer
       (save-excursion
	 (goto-char (point-max))
	 (insert (format jmail-search--format " "
			 (propertize (format-time-string "%F" date-str)
				     'face 'font-lock-comment-face)
			 (propertize (truncate-string-to-width from-str 16)
				     'face 'font-lock-variable-name-face)
			 subject))
	 (setq object (plist-put object 'jmail-nntp-article article))
	 (add-text-properties (line-beginning-position) (line-end-position) object)
	 (insert "\n"))))))

(defun jmail-nntp--display-header (articles)
  (with-current-buffer jmail-nntp--process-buffer-name
    (erase-buffer)
    (nntp-retrieve-headers articles jmail-nntp-group)
    (goto-char (point-max))
    (forward-line -1)
    (while (not (eq (point) (point-min)))
      (let ((line (buffer-substring (line-beginning-position)
				    (line-end-position))))
	(jmail-nntp--insert-header line))
      (forward-line -1))))

(defun jmail-nntp--display-headers (&optional force-display)
  (with-jmail-nntp-buffer
   (let ((cur-lines (count-lines (window-start) (point-max)))
	 (win-lines (window-total-height)))
     (when (and (not (zerop jmail-nntp--remainings-articles))
		(or (< cur-lines win-lines) force-display))
       (let* ((diff (if force-display
			win-lines
		      (- win-lines cur-lines)))
	      (steps (if (> jmail-nntp--remainings-articles diff)
			 diff
		       jmail-nntp--remainings-articles))
	      (articles (number-sequence
			 jmail-nntp--remainings-articles
			 (- jmail-nntp--remainings-articles steps) -1)))
	 (jmail-nntp--display-header (reverse articles))
	 (setq jmail-nntp--remainings-articles
	       (- jmail-nntp--remainings-articles steps)))))))

(defun jmail-nntp--display-summary ()
  (setq jmail-nntp--remainings-articles 0)
  (unless (get-buffer jmail-nntp--buffer-name)
    (with-current-buffer (get-buffer-create jmail-nntp--buffer-name)
      (jmail-nntp-mode)))
  (with-jmail-nntp-buffer
   (erase-buffer)
   (if-let ((nbr (jmail-nntp--get-nbr-articles)))
       (progn
	 (setq jmail-nntp--remainings-articles nbr)
	 (jmail-nntp--display-headers))
     (insert (propertize "No articles found !!!!"
			 'face 'jmail-search-results-footer-face)))
   (switch-to-buffer (current-buffer))))

(defun jmail-nntp--get-article (article)
  (with-current-buffer jmail-nntp--process-buffer-name
    (erase-buffer)
    (nntp-retrieve-articles (list article) jmail-nntp-group)
    (message-goto-body)
    (list (message-fetch-field "From")
	  (message-fetch-field "To")
	  (message-fetch-field "Cc")
	  (message-fetch-field "Newsgroups")
	  (message-fetch-field "Subject")
	  (message-fetch-field "Date")
	  (buffer-substring (point) (point-max)))))

(defun jmail-nntp--display-article (article)
  (cl-multiple-value-bind (from to cc mailing-list subject date body)
      (jmail-nntp--get-article article)
    (cl-macrolet ((insert-header (field)
		   `(when ,field
		      (message-insert-header ',field ,field)
		      (insert "\n"))))
      (insert-header from)
      (insert-header to)
      (insert-header cc)
      (insert-header mailing-list)
      (insert-header subject)
      (insert-header date))
    (insert body)
    (setq jmail-view--html-view nil)
    (jmail-view--fontify-mail)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun jmail-nntp--after-scroll (win _start)
  (jmail-nntp--display-headers))

;;; External Functions

(defun jmail-nntp-quit ()
  (interactive)
  (jmail-view-quit)
  (kill-buffer jmail-nntp--buffer-name)
  (kill-buffer jmail-nntp--process-buffer-name)
  (jmail))

(defun jmail-nntp-display-all ()
  (interactive)
  (with-jmail-nntp-buffer
   (let ((total jmail-nntp--remainings-articles))
     (while (not (zerop jmail-nntp--remainings-articles))
       (jmail-nntp--display-headers t)
       (message "Display all results: %d%%"
		(/ (* (- total jmail-nntp--remainings-articles) 100)
		   total))))))

(defun jmail-nntp-refresh ()
  (interactive)
  (jmail-nntp--display-summary))

(defun jmail-nntp-enter ()
  (interactive)
  (when-let ((article (get-text-property (point) 'jmail-nntp-article)))
    (if (get-buffer jmail-view--buffer-name)
	(switch-to-buffer-other-window jmail-view--buffer-name)
      (jmail-view--setup-buffer (current-buffer)))
    (with-jmail-view-buffer
     (setq jmail-view--html-view jmail-view-html-default-view)
     (jmail-nntp--display-article article))))

(defun jmail-nntp ()
  (interactive)
  (when (nntp-open-server jmail-nntp-server)
    (setq nntp-server-buffer (get-buffer-create jmail-nntp--process-buffer-name))
    (unless jmail-nntp--groups
      (setq jmail-nntp--groups (jmail-nntp--get-groups)))
    (if (assoc jmail-nntp-group jmail-nntp--groups)
	(jmail-nntp--display-summary)
      (message "Group %s not found" (propertize jmail-nntp-group 'face 'error)))))

(provide 'jmail-nntp)
