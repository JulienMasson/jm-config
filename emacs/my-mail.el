;;; my-mail.el --- Mail Configuration

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

;; default message from style
(setq message-from-style 'angles)

;; default citation format
(setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; message buffer will be killed after sending a message
(setq message-kill-buffer-on-exit t)

;; default config when sending mail
(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail
      smtpmail-debug-info nil
      mail-setup-hook nil
      sendmail-program "/usr/bin/msmtp")

;; utils to get a mail agent function
(defun get-mail-agent-function (string)
  (let ((backend (replace-regexp-in-string
		  "-user-agent" ""
		  (symbol-name mail-user-agent))))
    (intern (concat backend string))))

;; org msg
(require 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil \\n:t")
(setq org-msg-startup "hidestars noindent inlineimages")
(setq org-msg-greeting-fmt "\nHi %s,\n\n")
(setq org-msg-greeting-fmt-mailto t)
(setq org-msg-signature "

Regards,

#+begin_signature
--\n
Julien Masson
#+end_signature")
(org-msg-mode)

;; minimal diff face applied in current mail visited
(defun apply-minimal-diff-face-buffer ()
  (cl-flet ((apply-defface (min max face)
			   (let ((overlay (make-overlay min max)))
			     (overlay-put overlay 'face face))))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "^diff \-\-git" nil t))
      (while (not (eobp))
	(let* ((beg (point))
	       (end (line-end-position))
	       (str (buffer-substring-no-properties beg end)))
	  (cond ((string-match "^\\(---\\|\\+\\+\\+\\|@@\\)" str)
		 (apply-defface beg end 'diff-hunk-header))
		((string-match "^\\+" str)
		 (apply-defface beg end 'diff-added))
		((string-match "^\\-" str)
		 (apply-defface beg end 'diff-removed)))
	  (forward-line))))))

;; fold mail thread
(require 'hide-lines)

(defun mail-level-at-point ()
  (let ((fun (get-mail-agent-function "-level-at-point")))
    (if (symbol-function fun)
	(funcall fun)
      (error "`%s` not implemented !" fun))))

(defun mail-range-thread ()
  (save-excursion
    (cl-flet ((thread-pos (direction)
			  (while (and (mail-level-at-point)
				      (> (mail-level-at-point) 0))
			    (funcall direction))
			  (point)))
      (beginning-of-line)
      (let ((end (point))
	    (begin (thread-pos 'previous-line)))
	(next-line)
	(when (> (mail-level-at-point) 0)
	  (setq end (- (thread-pos 'forward-line) 1)))
	(unless (= begin end)
	  `(,begin ,end))))))

(defun mail-find-overlay-at-point ()
  (save-excursion
    (move-end-of-line nil)
    (seq-find (lambda (overlay)
		(let ((pos (point))
		      (end (overlay-end overlay)))
		  (= pos end)))
	      hide-lines-invisible-areas)))

(defun mail-unfold-one-thread (overlay)
  (setq hide-lines-invisible-areas
	(delete overlay hide-lines-invisible-areas))
  (delete-overlay overlay))

(defun mail-fold-one-thread ()
  (save-excursion
    (let ((range (mail-range-thread)))
      (when range
	(cl-multiple-value-bind (begin end)
	    range
	  (goto-char begin)
	  (hide-lines-add-overlay (line-end-position) end))))))

(defun mail-headers-fold-unfold-thread ()
  (interactive)
  (let ((overlay (mail-find-overlay-at-point)))
    (if overlay
	(mail-unfold-one-thread overlay)
      (mail-fold-one-thread))))

(defun mail-headers-fold-unfold-all ()
  (interactive)
  (save-excursion
    (if hide-lines-invisible-areas
	(hide-lines-show-all)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(mail-fold-one-thread)
	(next-line)))))

;; mail client
(require 'my-mu4e)
(require 'my-notmuch)


(provide 'my-mail)
