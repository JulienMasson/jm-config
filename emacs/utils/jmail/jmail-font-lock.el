;;; jmail-font-lock.el --- XXXX

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

(require 'diff-mode)
(require 'message)

;;; Faces

(defface jmail-font-lock-cited-0-face
  '((t :inherit font-lock-variable-name-face :bold nil :italic t))
  "Face for cited message parts (level 0)."
  :group 'jmail)

(defface jmail-font-lock-cited-1-face
  '((t :inherit font-lock-preprocessor-face :bold nil :italic t))
  "Face for cited message parts (level 1)."
  :group 'jmail)

(defface jmail-font-lock-cited-2-face
  '((t :inherit font-lock-constant-face :bold nil :italic t))
  "Face for cited message parts (level 2)."
  :group 'jmail)

(defface jmail-font-lock-cited-3-face
  '((t :inherit font-lock-function-name-face :bold nil :italic t))
  "Face for cited message parts (level 3)."
  :group 'jmail)

(defface jmail-font-lock-cited-4-face
  '((t :inherit font-lock-type-face :bold nil :italic t))
  "Face for cited message parts (level 4)."
  :group 'jmail)

(defface jmail-font-lock-cited-5-face
  '((t :inherit font-lock-comment-face :bold nil :italic t))
  "Face for cited message parts (level 5)."
  :group 'jmail)

(defface jmail-font-lock-cited-6-face
  '((t :inherit font-lock-comment-delimiter-face :bold nil :italic t))
  "Face for cited message parts (level 6)."
  :group 'jmail)

;;; Internal Functions

(defun jmail-font-lock--diff-beginning ()
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward "^diff \-\-git" nil t))
    (point)))

(defun jmail-font-lock--diff-matcher (regexp)
  `(lambda (limit)
     (let ((body-beg (jmail-eoh-mail-point))
	   (diff-beg (jmail-font-lock--diff-beginning))
	   (beg (point)))
       (and (> beg body-beg)
	    (> beg diff-beg)
	    (re-search-forward ,regexp limit t)))))

(defun jmail-font-lock--cited-face ()
  (when-let* ((str (buffer-substring (line-beginning-position) (point)))
	      (level (mod (string-width (replace-regexp-in-string
					 "[^>]" "" str)) 7)))
    (intern-soft (format "jmail-font-lock-cited-%d-face" level))))


;;; External Variables

(defvar jmail-font-lock-message
  (let ((content "[ \t]*\\(.+\\(\n[ \t].*\\)*\\)\n?"))
    `((message-match-to-eoh
       (,(concat "^\\([Tt]o:\\)" content)
	(progn (goto-char (match-beginning 0)) (match-end 0)) nil
	(1 'message-header-name)
	(2 'message-header-to nil t))
       (,(concat "^\\(^[GBF]?[Cc][Cc]:\\|^[Rr]eply-[Tt]o:\\)" content)
	(progn (goto-char (match-beginning 0)) (match-end 0)) nil
	(1 'message-header-name)
	(2 'message-header-cc nil t))
       (,(concat "^\\([Ss]ubject:\\)" content)
	(progn (goto-char (match-beginning 0)) (match-end 0)) nil
	(1 'message-header-name)
	(2 'message-header-subject nil t))
       (,(concat "^\\([Nn]ewsgroups:\\|Followup-[Tt]o:\\)" content)
	(progn (goto-char (match-beginning 0)) (match-end 0)) nil
	(1 'message-header-name)
	(2 'message-header-newsgroups nil t))
       (,(concat "^\\(X-[A-Za-z0-9-]+:\\|In-Reply-To:\\)" content)
	(progn (goto-char (match-beginning 0)) (match-end 0)) nil
	(1 'message-header-name)
	(2 'message-header-xheader))
       (,(concat "^\\([A-Z][^: \n\t]+:\\)" content)
	(progn (goto-char (match-beginning 0)) (match-end 0)) nil
        (1 'message-header-name)
        (2 'message-header-other nil t)))
      ,@(if (and mail-header-separator
		 (not (equal mail-header-separator "")))
	    `((,(concat "^\\(" (regexp-quote mail-header-separator) "\\)$")
	       1 'message-separator))
	  nil)
      ("<#/?\\(multipart\\|part\\|external\\|mml\\|secure\\)[^>]*>"
       (0 'message-mml)))))

(defvar jmail-font-lock-diff
  `((,(jmail-font-lock--diff-matcher "^\\(---\\|\\+\\+\\+\\).*")
     (0 'diff-file-header))
    (,(jmail-font-lock--diff-matcher "^@@.*")
     (0 'diff-header))
    (,(jmail-font-lock--diff-matcher "^\\+.*")
     (0 'diff-added))
    (,(jmail-font-lock--diff-matcher "^\\-.*")
     (0 'diff-removed))))

(defvar jmail-font-lock-cited
  `(((lambda (limit)
       (let ((body-beg (jmail-eoh-mail-point))
	     (regexp (concat "^\\(" message-cite-prefix-regexp "\\).*"))
	     (beg (point)))
	 (and (> beg body-beg)
	      (re-search-forward regexp limit t))))
     (0 (jmail-font-lock--cited-face)))))

(defvar jmail-font-lock (append jmail-font-lock-cited
				jmail-font-lock-diff
				jmail-font-lock-message))

(provide 'jmail-font-lock)
