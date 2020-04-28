;;; echat-ui.el --- XXXX

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

(require 'echat-logs)
(require 'lui)

;;; Customization

(defcustom echat-ui-secs-timeout (* 5 60)
  "Specify the number of seconds difference for which we should insert header."
  :group 'echat
  :type 'number)

;;; Faces

(defface echat-ui-info-face
  '((((class color) (background light)) :foreground "grey15")
    (((class color) (background  dark)) :foreground "grey30"))
  "Face for echat info msg"
  :group 'echat-faces)

(defface echat-ui-separator-face
  '((t :underline "grey30" :extend t))
  "Face for echat separator msg"
  :group 'echat-faces)

(defface echat-ui-unread-face
  '((((class color) (background light)) :foreground "coral4" :weight bold)
    (((class color) (background  dark)) :foreground "coral2" :weight bold))
  "Face for echat unread msg"
  :group 'echat-faces)

(defface echat-ui-unread-separator-face
  '((((class color) (background light)) :strike-through "coral4" :extend t)
    (((class color) (background  dark)) :strike-through "coral2" :extend t))
  "Face for echat unread separator msg"
  :group 'echat-faces)

(defface echat-ui-nick-face
  '((((class color) (background light)) :foreground "SpringGreen4" :weight bold)
    (((class color) (background  dark)) :foreground "SpringGreen3" :weight bold))
  "Face for echat nick msg"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-ui--clean-up (body)
  ;; force new line after point
  (replace-regexp-in-string "\\.\\s-+" ".\n" body))

(defun echat-ui--find-header ()
  (catch 'found
    (save-excursion
      (goto-char lui-output-marker)
      (while (not (bobp))
	(forward-line -1)
	(when-let* ((properties (text-properties-at (point)))
		    (header-p (and (plist-member properties :echat-time)
				   (plist-member properties :echat-nick))))
	  (throw 'found (point)))))))

(defun echat-ui--no-need-header-p (nick time)
  (catch 'found
    (save-excursion
      (goto-char lui-output-marker)
      (while (not (bobp))
	(forward-line -1)
	(when-let* ((properties (text-properties-at (point)))
		    (previous-time (plist-get properties :echat-time))
		    (previous-nick (plist-get properties :echat-nick))
		    (time-elapsed (time-subtract time previous-time)))
	  (throw 'found (and (string= previous-nick nick)
			     (< (time-to-seconds time-elapsed)
				echat-ui-secs-timeout))))))))

(defun echat-ui--insert-header (echat sender me time &optional icon)
  (let* ((face (oref echat face))
	 (myself (string= sender me))
	 (ts (format-time-string lui-time-stamp-format time
                                 lui-time-stamp-zone))
	 (icon-str (when icon (propertize "image" 'display icon)))
	 (icon-length (if icon 6 0))
	 (spaces (- lui-fill-column (length ts) icon-length))
	 (fmt (format "%%-%ds%%s" spaces))
	 (nick-face (if myself 'echat-ui-nick-face face))
	 (nick-str (propertize sender 'face nick-face))
	 (ts-str (propertize ts 'face 'lui-time-stamp-face))
	 (inhibit-read-only t)
	 (beg lui-output-marker))
    (save-excursion
      (goto-char lui-output-marker)
      (when icon-str (insert icon-str " "))
      (insert (format fmt nick-str ts-str))
      (add-text-properties lui-output-marker (point)
			   (list :echat-time time
				 :echat-nick sender))
      (insert "\n")
      (set-marker lui-output-marker (point)))))

(defun echat-ui--fill-body (beg end)
  (let ((inhibit-read-only t)
	(fill-column lui-fill-column)
	(cur beg))
    (save-excursion
      (goto-char end)
      (save-excursion
	(goto-char beg)
	(while (and (re-search-forward "\n" nil t)
		    (< (point) end))
	  (fill-region cur (point) 'left)
	  (setq cur (point))))
      (set-marker lui-output-marker (point)))))

(defun echat-ui--unread-separator ()
  (catch 'found
    (save-excursion
      (goto-char (point-max))
      (goto-char (line-beginning-position))
      (while (not (bobp))
	(forward-line -1)
	(dolist (ov (overlays-at (point)))
	  (when (eq (overlay-get ov 'face) 'echat-ui-unread-separator-face)
	    (throw 'found (point))))))))

(defun echat-ui--remove-unread-separator ()
  (save-excursion
    (goto-char lui-output-marker)
    (save-excursion
      (when-let ((pos (echat-ui--unread-separator))
		 (inhibit-read-only t))
	(goto-char pos)
	(mapc #'delete-overlay (overlays-at pos))
	(delete-region pos (+ (line-end-position) 1))))
    (set-marker lui-output-marker (point))))

(defun echat-ui--insert-unread-separator (count)
  (let* ((str (format "| %s unread messages |" count))
	 (spaces-before (- (/ lui-fill-column 2) (/ (length str) 2)))
	 (spaces-after (- lui-fill-column spaces-before))
	 (fmt (format "%%-%ds%%-%ds" spaces-before spaces-after))
	 (beg (line-beginning-position))
	 (inhibit-read-only t))
    (insert (format fmt "" (propertize str 'face 'echat-ui-unread-face)))
    (overlay-put (make-overlay beg (+ beg spaces-before))
                 'face 'echat-ui-unread-separator-face)
    (overlay-put (make-overlay (+ beg spaces-before (length str))
			       (+ beg lui-fill-column))
                 'face 'echat-ui-unread-separator-face)))

(defun echat-ui--update-unread-separator (echat)
  (when-let* ((echat-buffer (echat-find-echat-buffer (current-buffer)))
	      (unread-count (oref echat-buffer unread-count))
	      (inhibit-read-only t))
    (unless (zerop unread-count)
      (save-excursion
	(if-let ((pos (echat-ui--unread-separator)))
	    (progn
	      (goto-char pos)
	      (mapc #'delete-overlay (overlays-at pos))
	      (delete-region pos (line-end-position))
	      (echat-ui--insert-unread-separator unread-count))
	  (goto-char lui-output-marker)
	  (echat-ui--insert-unread-separator unread-count)
	  (insert "\n")
	  (set-marker lui-output-marker (point)))))))

(defun echat-ui--remove-separator ()
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward lui-prompt-string nil t)
      (let ((end (point)))
	(forward-line -2)
	(when-let ((inhibit-read-only t)
		   (overlay (cl-find-if (lambda (ov)
					  (eq (overlay-get ov 'face)
					      'echat-ui-separator-face))
					(overlays-at (point)))))
	  (delete-overlay overlay)
	  (delete-region (point) end)
	  (set-marker lui-output-marker (point)))))))

(defun echat-ui--insert-separator ()
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward lui-prompt-string nil t)
      (let* ((inhibit-read-only t)
	     (beg (line-beginning-position))
	     (end (+ beg lui-fill-column))
	     (spaces (make-string lui-fill-column (string-to-char " "))))
	(insert spaces "\n\n")
	(overlay-put (make-overlay beg end)
                     'face 'echat-ui-separator-face))
      (set-marker lui-output-marker (point)))))

(defun echat-ui--insert-msg (echat sender me body time icon)
  (echat-ui--update-unread-separator echat)
  (unless (echat-ui--no-need-header-p sender time)
    (echat-ui--insert-header echat sender me time icon))
  (let ((beg (marker-position lui-output-marker)))
    (lui-insert (propertize (concat (echat-ui--clean-up body) "\n")
			    'lui-format-argument 'body))
    (echat-ui--fill-body beg (marker-position lui-output-marker))))

;;; External Functions

(defun echat-ui-remove-unread-separator (buffer)
  (when (echat-find-by-buffer buffer)
    (with-current-buffer buffer
      (echat-ui--remove-unread-separator))))

(defun echat-ui-goto-unread-messages ()
  (interactive)
  (when (and (derived-mode-p 'lui-mode)
	     (echat-find-by-buffer (current-buffer)))
    (when-let ((pos (echat-ui--unread-separator)))
      (goto-char pos))))

(defun echat-ui-setup-buffer (echat me buffer)
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (echat-logs-insert-load-more echat me)
      (set-marker lui-output-marker (point))
      (lui-set-prompt lui-prompt-string)
      (echat-ui--insert-separator)
      (goto-char (point-max)))))

(defun echat-ui-insert-info (echat body)
  (echat-ui--remove-separator)
  (let ((beg (marker-position lui-output-marker)))
    (lui-insert (propertize (concat body "\n") 'face 'echat-ui-info-face))
    (echat-ui--fill-body beg (marker-position lui-output-marker)))
  (echat-ui--insert-separator))

(cl-defun echat-ui-insert-msg (echat sender me body &key icon save
				     (time (current-time)))
  (echat-ui--remove-separator)
  (when save (echat-logs-save echat sender body time))
  (echat-ui--insert-msg echat sender me body time icon)
  (echat-ui--insert-separator))

(provide 'echat-ui)
