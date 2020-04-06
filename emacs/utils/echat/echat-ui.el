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

(defface echat-ui-nick-face
  '((((class color) (background light)) :foreground "SpringGreen4")
    (((class color) (background  dark)) :foreground "SpringGreen3"))
  "Face for echat nick msg"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-ui--clean-up (body)
  (replace-regexp-in-string "\\.\\s-+" ".\n" body))

(defun echat-ui--fill (beg justify)
  (let ((inhibit-read-only t)
	(fill-column lui-fill-column)
	(cur beg))
    (goto-char beg)
    (while (re-search-forward "\n" nil t)
      (fill-region cur (point) justify)
      (setq cur (point)))))

(defun echat-ui--need-header-p (nick)
  (catch 'found
    (save-excursion
      (while (not (bobp))
	(when-let* ((properties (text-properties-at (point)))
		    (previous-time (plist-get properties :echat-time))
		    (previous-nick (plist-get properties :echat-nick))
		    (time-elapsed (time-subtract (current-time)
						 previous-time)))
	  (throw 'found (not (and (string= previous-nick nick)
				  (< (time-to-seconds time-elapsed)
				     echat-ui-secs-timeout)))))
	(forward-line -1))
      t)))

(defun echat-ui--insert-header (echat sender me time &optional icon)
  (let* ((face (oref echat face))
	 (myself (string= sender me))
	 (ts (format-time-string lui-time-stamp-format time
                                 lui-time-stamp-zone))
	 (icon-str (when icon (propertize "image" 'display icon)))
	 (icon-length (if icon 5 0))
	 (spaces (- (/ lui-fill-column 2) (/ (length ts) 2)))
	 (fmt (format "%%-%ds%%s%%%ds"
		      (if myself spaces (- spaces icon-length))
		      (if myself (- spaces icon-length) spaces)))
	 (nick-face (if myself 'echat-ui-nick-face face))
	 (nick-str (propertize sender 'face `(bold ,nick-face)))
	 (ts-str (propertize ts 'face 'lui-time-stamp-face))
	 (inhibit-read-only t)
	 (beg (point)))
    (save-excursion
      (goto-char lui-output-marker)
      (when (and icon-str (not myself))
	(insert icon-str " "))
      (insert (format fmt (if myself "" nick-str) ts-str
		      (if myself nick-str "")))
      (when (and icon-str myself)
	(insert " " icon-str))
      (insert "\n")
      (set-marker lui-output-marker (point))
      (goto-char beg)
      (add-text-properties (line-beginning-position)
			   (line-end-position)
			   (list :echat-time time
				 :echat-nick sender)))))

(defun echat-ui--remove-last-separator ()
  (save-excursion
    (goto-char lui-output-marker)
    (forward-line -2)
    (when-let* ((beg (line-beginning-position))
		(overlays (overlays-at beg)))
      (mapc #'delete-overlay overlays)
      (delete-region beg (+ (line-end-position) 1)))))

(defun echat-ui--insert-separator ()
  (save-excursion
    (goto-char lui-output-marker)
    (forward-line -1)
    (let* ((inhibit-read-only t)
	   (beg (line-beginning-position))
	   (end (+ beg lui-fill-column))
	   (spaces (make-string lui-fill-column (string-to-char " "))))
      (insert spaces "\n")
      (overlay-put (make-overlay beg end)
                   'face 'echat-ui-separator-face))))

;;; External Functions

(defun echat-ui-insert-info (echat body)
  (echat-ui--remove-last-separator)
  (let ((beg (point)))
    (lui-insert (propertize (concat body "\n") 'face 'echat-ui-info-face))
    (echat-ui--fill beg 'left))
  (echat-ui--insert-separator))

(cl-defun echat-ui-insert-msg (echat sender me body &key icon save
				     (time (current-time)))
  (echat-ui--remove-last-separator)
  (when save (echat-logs-save echat sender body time))
  (when (echat-ui--need-header-p sender)
    (echat-ui--insert-header echat sender me time icon))
  (let ((beg (point)))
    (lui-insert (propertize (concat (echat-ui--clean-up body) "\n")
			    'lui-format-argument 'body))
    (echat-ui--fill beg (if (string= sender me) 'right 'left)))
  (echat-ui--insert-separator)
  (goto-char (point-max)))

(provide 'echat-ui)
