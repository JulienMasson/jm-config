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
  '((((class color) (background light)) :foreground "SpringGreen4" :weight bold)
    (((class color) (background  dark)) :foreground "SpringGreen3" :weight bold))
  "Face for echat nick msg"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-ui--clean-up (body)
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
	 (icon-length (if icon 5 0))
	 (spaces (- (/ lui-fill-column 2) (/ (length ts) 2)))
	 (fmt (format "%%-%ds%%s%%%ds"
		      (if myself spaces (- spaces icon-length))
		      (if myself (- spaces icon-length) spaces)))
	 (nick-face (if myself 'echat-ui-nick-face face))
	 (nick-str (propertize sender 'face nick-face))
	 (ts-str (propertize ts 'face 'lui-time-stamp-face))
	 (inhibit-read-only t)
	 (beg lui-output-marker))
    (save-excursion
      (goto-char lui-output-marker)
      (when (and icon-str (not myself))
	(insert icon-str " "))
      (insert (format fmt (if myself " " nick-str) ts-str
		      (if myself nick-str " ")))
      (when (and icon-str myself)
	(insert " " icon-str))
      (add-text-properties lui-output-marker (point)
			   (list :echat-time time
				 :echat-nick sender))
      (insert "\n")
      (set-marker lui-output-marker (point)))))

(defun echat-ui--fill-body (beg end justify)
  (let ((inhibit-read-only t)
	(fill-column lui-fill-column)
	(cur beg))
    (save-excursion
      (goto-char end)
      (save-excursion
	(goto-char beg)
	(while (and (re-search-forward "\n" nil t)
		    (< (point) end))
	  (fill-region cur (point) justify)
	  (setq cur (point))))
      (set-marker lui-output-marker (point)))))

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
  (unless (echat-ui--no-need-header-p sender time)
    (echat-ui--insert-header echat sender me time icon))
  (let ((beg (marker-position lui-output-marker)))
    (lui-insert (propertize (concat (echat-ui--clean-up body) "\n")
			    'lui-format-argument 'body))
    (echat-ui--fill-body beg (marker-position lui-output-marker)
			 (if (string= sender me) 'right 'left))))

;;; External Functions

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
    (echat-ui--fill-body beg (marker-position lui-output-marker) 'left))
  (echat-ui--insert-separator))

(cl-defun echat-ui-insert-msg (echat sender me body &key icon save
				     (time (current-time)))
  (echat-ui--remove-separator)
  (when save (echat-logs-save echat sender body time))
  (echat-ui--insert-msg echat sender me body time icon)
  (echat-ui--insert-separator))

(provide 'echat-ui)
