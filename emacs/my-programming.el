;;; my-programming.el --- Programming Configuration

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

;; jump only on error compilation
(setq compilation-skip-threshold 2)

;; ansi color on compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; visual regexp
(require 'visual-regexp)

;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; save backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; auto-detection indenting
(require 'dtrt-indent)
(dtrt-indent-mode t)

;; change default grep
(setq grep-command "grep --color -nsrH -E ")

;; grep context
(require 'grep-context)
(add-hook 'compilation-mode-hook #'grep-context-mode)

(defun my-grep-context (&optional arg)
  (interactive "p")
    (cond ((= arg 1) (call-interactively 'grep-context-more-around-point))
        (t (call-interactively 'grep-context-less-around-point))))

;; grep save buffer
(defun grep-save-buffer ()
  (interactive)
  (rename-buffer (generate-new-buffer-name "*grep*")))

;; grep at point
(require 'thingatpt)
(defun grep-at-point ()
  (interactive)
  (grep (format "%s%s ."
		grep-command
		(thing-at-point 'symbol))))

;; occur at point
(defun occur-at-point ()
  (interactive)
  (occur (thing-at-point 'symbol)))

;; ripgrep
(require 'rg)

;; company mode
(require 'company)
(setq company-backends nil)
(add-hook 'after-init-hook 'global-company-mode)

;; completion at point company
(require 'company-capf)
(add-to-list 'company-backends 'company-capf)

;; ;; files company
(require 'company-files)
(add-to-list 'company-backends 'company-files)

;; perl
(defalias 'perl-mode 'cperl-mode)

;; gdb
(require 'my-gdb)

;; C source code
(require 'my-c)

;; python source code
(require 'my-python)

;; manual at point
(defun manual-at-point()
  (interactive)
  (cond
   ;; C mode
   ((string= major-mode "c-mode")
    (let* ((pattern (thing-at-point 'symbol))
	   (man-result (split-string
			(shell-command-to-string (format "man -f %s" pattern)) "\n"))
	   (match (car (delq nil
			     (mapcar (lambda (line)
				       (if (string-match (format "\\(%s ([23])\\).*" pattern) line)
					   (match-string 1 line)))
				     man-result)))))
      (if match
	  (man match))))
   ;; Perl mode
   ((string= major-mode "cperl-mode")
    (cperl-perldoc-at-point))
   ;; Python mode
   ((string= major-mode "python-mode")
    (anaconda-mode-show-doc))
   ;; Emacs lisp mode
   ((or (string= major-mode "emacs-lisp-mode")
	(string= major-mode "lisp-interaction-mode"))
    (let* ((string (thing-at-point 'symbol))
	   (symbol (intern-soft string)))
      (when symbol
	(cond ((fboundp symbol)
	       (describe-function symbol))
	      ((boundp symbol)
	       (describe-variable symbol))))))))


(provide 'my-programming)
