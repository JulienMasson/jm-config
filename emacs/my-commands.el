;;; my-edit.el --- Commands Configuration

;; Copyright (C) 2020 Julien Masson

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

;; jump to newline
(defun jump-newline-and-indent ()
  (interactive "*")
  (end-of-line)
  (newline-and-indent))

;; right/left symbol
(defun right-symbol ()
  (interactive)
  (forward-symbol 1))

(defun left-symbol ()
  (interactive)
  (forward-symbol -1))

;; right/left balanced expression
(defun right-sexp ()
  (interactive)
  (forward-sexp 1))

(defun left-sexp ()
  (interactive)
  (forward-sexp -1))

;; kill commands
(defun kill-word-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-word)))

;; delete commands
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(defun delete-char-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-region)
    (call-interactively 'delete-char)))

(defun delete-sexp (&optional arg)
  (interactive "p")
  (let ((opoint (point)))
    (forward-sexp (or arg 1))
    (delete-region opoint (point))))

;; copy commands
(defun show-and-copy-buffer-filename ()
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun copy-line ()
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position)))

(defun copy-word ()
  (interactive)
  (kill-ring-save (save-excursion (left-symbol) (point))
                  (save-excursion (right-symbol) (point))))

(defun kill-ring-save-or-copy-line ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (call-interactively 'copy-line)))

;; isearch commands
(defun isearch-yank-beginning (&optional arg)
  (interactive "p")
  (setq isearch-string "")
  (setq isearch-message "")
  (isearch-search-and-update))

(defun isearch-delete-selection ()
  (interactive)
  (delete-region isearch-other-end (point))
  (isearch-done))

(defun isearch-kill-selection ()
  (interactive)
  (kill-region isearch-other-end (point))
  (isearch-done))

(defun isearch-yank-symbol-or-char ()
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (if (or (memq (char-syntax (or (char-after)  0)) '(?w ?_))
	     (memq (char-syntax (or (char-after (1+ (point)))  0)) '(?w ?_)))
	 (if (and (boundp 'subword-mode)  subword-mode) (subword-forward 1) (forward-symbol 1))
       (forward-char 1))
     (point))))

(defun isearch-kill-ring-save ()
  (interactive)
  (kill-ring-save isearch-other-end (point))
  (isearch-done)
  (goto-char isearch-other-end))

;; occur at point
(defun occur-at-point ()
  (interactive)
  (occur (thing-at-point 'symbol)))

;; window dedicated
(defun toggle-window-dedicated ()
  (interactive)
  (let (window (get-buffer-window (current-buffer)))
    (set-window-dedicated-p window (not (window-dedicated-p window)))))

;; toggle window split
(defun toggle-window-split ()
  (interactive)
  (let ((split (frame-parameter nil 'unsplittable)))
    (set-frame-parameter nil 'unsplittable (not split))))

;; swap last buffers
(defun swap-last-buffers ()
  (interactive)
  (let ((current (get-buffer-window (current-buffer)))
        (last (get-buffer-window (nth 1 (buffer-list)))))
    (window-swap-states current last)))

;; find file as root
(defun sudo-find-file (file)
  (interactive (list (read-file-name "Find file (as root): ")))
  (if (tramp-tramp-file-p file)
      (let* ((dissect (tramp-dissect-file-name file))
	     (host (tramp-file-name-host dissect))
	     (local-file (tramp-file-name-localname dissect))
	     (tramp (replace-regexp-in-string local-dir "" dir)))
	(find-alternate-file (format "%s|sudo:root@%s:%s"
				     tramp host local-file)))
    (find-alternate-file (concat "/sudo:root@localhost:" file))))

(provide 'my-commands)
