;;; my-mu4e.el --- Mu4e Configuration

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

;; load mu4e module
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; maildir root path
(setq mu4e-maildir "~/Maildir")

;; command to fetch mail
(setq mu4e-get-mail-command "offlineimap")

;; hide index messages
(setq mu4e-hide-index-messages t)

;; maximum number of results to show
(setq mu4e-headers-results-limit 100)

;; descending sorting
(setq mu4e-headers-sort-direction 'descending)

(defun mu4e-headers-toggle-sort-direction ()
  (interactive)
  (if (eq mu4e-headers-sort-direction 'descending)
      (setq mu4e-headers-sort-direction 'ascending)
    (setq mu4e-headers-sort-direction 'descending))
  (mu4e-headers-rerun-search))

;; fancy chars
(setq mu4e-use-fancy-chars t)

;; time (secs) to retrieve mail and update the database
(setq mu4e-update-interval 60)

;; header child marks
(setq mu4e-headers-thread-child-prefix '("  ├>" . "  ┣▶ "))
(setq mu4e-headers-thread-last-child-prefix '("  └>" . "  ┗▶ "))
(setq mu4e-headers-thread-connection-prefix '("  │" . "  ┃ "))
(setq mu4e-headers-thread-orphan-prefix '("  ┬>" . "  ┳▶ "))
(setq mu4e-headers-thread-single-orphan-prefix '("  ─>" . "  ━▶ "))
(setq mu4e-headers-thread-duplicate-prefix '("  =" . "  ≡ "))

;; write own main view
(defvar my-mu4e~main-buffer-name "*mail*")
(defun mu4e~main-view ()
  (let ((buf (get-buffer-create my-mu4e~main-buffer-name))
	(inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       "\n"
       (propertize "  Help Commands\n\n" 'face 'mu4e-title-face)
       (mu4e~main-action-str "\t* [j]ump to some maildir\n" 'mu4e-jump-to-maildir)
       (mu4e~main-action-str "\t* [s]earch query\n" 'mu4e-search)
       (mu4e~main-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new)
       (mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit))
      (mu4e-main-mode)
      (goto-char (point-min))
      (pop-to-buffer-same-window buf))
    (add-to-list 'global-mode-string '(:eval (mu4e-context-label)))))

;; default action in mu4e view and header
(setq mu4e-view-actions '(("show this thread" . mu4e-action-show-thread)))
(setq mu4e-headers-actions '(("show this thread" . mu4e-action-show-thread)))

;; add Apply patch action in mu4e view and header
(defun mu4e-action-git-apply-mbox (msg)
  (let ((default-directory (ido-read-directory-name "Directory: "))
	(path (mu4e-message-field msg :path)))
    (magit-run-git "am" path)))

(add-to-list 'mu4e-view-actions '("apply patch" . mu4e-action-git-apply-mbox))
(add-to-list 'mu4e-headers-actions '("apply patch" . mu4e-action-git-apply-mbox))

;; add Show message id action in mu4e view and header
(defun mu4e-action-show-message-id (msg)
  (let ((message-id (mu4e-message-field msg :message-id)))
    (message "%s" message-id)
    (kill-new message-id)))

(add-to-list 'mu4e-view-actions '("Show Message-Id" . mu4e-action-show-message-id))
(add-to-list 'mu4e-headers-actions '("Show Message-Id" . mu4e-action-show-message-id))

;; change headers
(setq mu4e-headers-date-format "%d %b")
(setq mu4e-headers-fields '((:date          .  8)
			    (:from          .  20)
			    (:subject       .  nil)))
(setq mu4e-view-fields '(:from :to :cc :subject :date :mailing-list :attachments :signature))

;; re-defun mu4e headers field function to display color on date and author
(defun mu4e~headers-field-apply-basic-properties (msg field val width)
  (case field
    (:subject
     (concat
      (mu4e~headers-thread-prefix (mu4e-message-field msg :thread))
      (truncate-string-to-width val 600)))
    (:thread-subject (mu4e~headers-thread-subject msg))
    ((:maildir :path :message-id) val)
    ((:to :from :cc :bcc) (propertize (mu4e~headers-contact-str val)
				      'face 'mu4e-contact-face))
    (:from-or-to (mu4e~headers-from-or-to msg))
    (:date (propertize (format-time-string mu4e-headers-date-format val)
		       'face 'mu4e-header-key-face))
    (:mailing-list (mu4e~headers-mailing-list val))
    (:human-date (propertize (mu4e~headers-human-date msg)
			     'help-echo (format-time-string
					 mu4e-headers-long-date-format
					 (mu4e-msg-field msg :date))))
    (:flags (propertize (mu4e~headers-flags-str val)
			'help-echo (format "%S" val)))
    (:tags (propertize (mapconcat 'identity val ", ")))
    (:size (mu4e-display-size val))
    (t (mu4e~headers-custom-field msg field))))

;; enable inline images
(setq mu4e-view-show-images t)

;; body display on the html-version
(setq mu4e-view-prefer-html t)

;; show full addresses in view message
(setq mu4e-view-show-addresses 't)

;; shell command used to converts from html to plain text
(setq mu4e-html2text-command 'mu4e-shr2text)

;; handle multi maildir accounts
(require 'mu4e-maildirs-extension)
(setq mu4e-maildirs-extension-buffer-name my-mu4e~main-buffer-name
      mu4e-maildirs-extension-maildir-indent 0
      mu4e-maildirs-extension-maildir-default-prefix " "
      mu4e-maildirs-extension-maildir-collapsed-prefix " "
      mu4e-maildirs-extension-maildir-expanded-prefix "*"
      mu4e-maildirs-extension-updating-string ""
      mu4e-maildirs-extension-maildir-format "\t%i%p %-20n (%u/%t)"
      mu4e-maildirs-extension-maildir-hl-regex mu4e-maildirs-extension-maildir-format)

;; update cache and summary after index updated even if window in background
(defun jm-mu4e-maildirs-extension-index-updated-handler ()
  (setq mu4e-maildirs-extension-bookmarks nil)
  (setq mu4e-maildirs-extension-maildirs nil)
  (mu4e-maildirs-extension-update)
  (mu4e-maildirs-extension-unqueue-maybe))

(setq mu4e-maildirs-extension-index-updated-func
      'jm-mu4e-maildirs-extension-index-updated-handler)

;; load mu4e maildir extension
(mu4e-maildirs-extension)

;; insert maildir header above "Help Commands" header
(setq mu4e-maildirs-extension-insert-before-str "\n  Help Commands")

;; don't display action on maildir header
(setq mu4e-maildirs-extension-action-text nil)

;; change title
(setq mu4e-maildirs-extension-title "  Accounts\n")

;; don't display some folders
(setq mu4e-maildirs-extension-ignored-regex "\\(^/drafts$\\|^/sent$\\)")

;; set account settings
(defun my-mu4e-set-account ()
  (when mu4e-compose-parent-message
    (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
      (string-match "/\\(.*?\\)/" maildir)
      (mail-set-vars (match-string 1 maildir)))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; ido attachment
(defstruct mu4e-attachments
  name
  index
  docid)

(defun mu4e-build-attachments-list ()
  (let* ((msg (mu4e-message-at-point))
	 (docid (mu4e-message-field msg :docid)))
    (mapcar (lambda (count)
	      (let* ((att (mu4e~view-get-attach msg count))
		     (name (plist-get att :name))
		     (index (plist-get att :index)))
		(make-mu4e-attachments :name name
				       :index index
				       :docid docid)))
	    (number-sequence 1
			     (hash-table-count mu4e~view-attach-map)))))

(defun mu4e-save-attachment (file data)
  (let ((docid (mu4e-attachments-docid data))
	(index (mu4e-attachments-index data)))
    (mu4e~proc-extract 'save docid index mu4e-decryption-policy file)))

(defun mu4e-ido-save-attachments ()
  (interactive)
  (let* ((attachments (mu4e-build-attachments-list))
	 (target (ido-completing-read "Save: "
				      (append '("all")
					      (mapcar (lambda (data)
							(mu4e-attachments-name data))
						      attachments))
				      nil t nil nil))
	 (dir (ido-read-directory-name "Directory: ")))
    (if (string= target "all")
	(mapc (lambda (data)
		(let ((target (mu4e-attachments-name data)))
		  (mu4e-save-attachment (expand-file-name target dir)
					data)))
	      attachments)
      (let ((file (expand-file-name target dir))
	    (data (find target attachments :key 'mu4e-attachments-name :test 'string=)))
	(mu4e-save-attachment file data)))))

;; org store link
(require 'org-mu4e)

;; fold mail thread
(require 'mu4e-message)

(defun mu4e-level-at-point ()
  (let* ((msg (mu4e-message-at-point))
	 (thread (plist-get msg :thread)))
    (plist-get thread :level)))

;; appply diff face in view mode
(defun mu4e-view-apply-diff-face (msg)
  (when (get-buffer mu4e~view-buffer-name)
    (with-current-buffer mu4e~view-buffer-name
      (apply-minimal-diff-face-buffer))))
(advice-add 'mu4e-view :after #'mu4e-view-apply-diff-face)

;; send kernel patchs
(require 'kernel-patch)
(setq kernel-patch-user-email "Julien Masson <jmasson@baylibre.com>")

(defun mu4e-kernel-patch ()
  ;; mu4e faces and completion
  (mu4e~compose-remap-faces)
  (mu4e~compose-setup-completion)

  ;; diff face
  (apply-minimal-diff-face-buffer)

  ;; set draft internal vars
  (set (make-local-variable 'mu4e~draft-drafts-folder)
       (mu4e-get-drafts-folder))
  (put 'mu4e~draft-drafts-folder 'permanent-local t)

  ;; set default smtp settings
  (setq-local message-sendmail-extra-arguments
	      '("-a" "baylibre")))

(add-hook 'kernel-patch-compose-hook 'mu4e-kernel-patch)

;; add Apply patch list action in mu4e header
(defun mu4e-get-patch-list ()
  (let (patch-list)
    (save-excursion
      (let ((range (mu4e-range-thread)))
	(when range
	  (cl-multiple-value-bind (begin end)
	      range
	    (goto-char begin)
	    (while (< (point) end)
	      (let* ((msg (mu4e-message-at-point))
		     (subject (plist-get msg :subject))
		     (level (plist-get (plist-get msg :thread) :level))
		     (path (plist-get msg :path)))
		(when (and (= level 1)
			   (string-match "^\\[PATCH" subject))
		  (add-to-list 'patch-list `(,subject . ,path)))
		(next-line)))))))
    (when patch-list
      (cl-sort patch-list (lambda (a b)
			    (string< (car a) (car b)))))))

(defun mu4e-apply-patch-list (msg)
  (let ((patch-list (mu4e-get-patch-list))
	(default-directory (ido-read-directory-name "Directory: ")))
    (when patch-list
      (mapc (lambda (patch)
	      (magit-run-git "am" (cdr patch)))
	    patch-list))))

(add-to-list 'mu4e-headers-actions '("Apply patch list" . mu4e-apply-patch-list))


(provide 'my-mu4e)
