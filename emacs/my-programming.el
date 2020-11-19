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

;; send command to compilation buffer
(defun compilation-send-command ()
  (interactive)
  (if-let ((process (get-buffer-process (current-buffer)))
	   (cmd (concat (read-string "Command: ") "\n"))
	   (inhibit-read-only t))
      (save-excursion
	(goto-char (process-mark process))
	(insert-before-markers cmd)
	(process-send-string process cmd))
    (error "Process not running")))

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

;; grep config
(require 'my-grep)

;; search config
(require 'my-search)

;; company mode
(require 'company)
(setq company-backends nil)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-minimum-width 40)
(global-company-mode)

;; company box
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; don't message in echo area
(defun company-echo-show (&optional getter))

;; completion at point company
(require 'company-capf)
(add-to-list 'company-backends 'company-capf)

;; files company
(require 'company-files)
(add-to-list 'company-backends 'company-files)

;; yaml
(require 'yaml-mode)

;; gdb
(require 'my-gdb)

;; C source code
(require 'my-c)

;; perl
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'acscope-database-default-files "pl")
(add-to-list 'acscope-mode-hook-list 'cperl-mode-hook)

;; anaconda mode
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)

;; anaconda company
(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)

;; manual at point
(defun man-get-index-list (pattern)
  (let* ((cmd (format "man -f %s" pattern))
	 (results (split-string (shell-command-to-string cmd) "\n"))
	 (regex (format "^%s (\\([0-9]\\)).*$" pattern)))
    (delq nil (mapcar (lambda (line)
			(when (string-match regex line)
			  (string-to-number (match-string 1 line))))
		      results))))

(defun man-search-index (pattern search-list)
  (let ((index-list (man-get-index-list pattern)))
    (seq-find (lambda (index)
		(member index index-list))
	      search-list)))

(defun manual-at-point()
  (interactive)
  (cond
   ;; C mode
   ((string= major-mode "c-mode")
    (let* ((pattern (thing-at-point 'symbol))
	   (search-list '(2 3))
	   (index (man-search-index pattern search-list)))
      (when index
	(man (format "%s (%d)" pattern index)))))
   ;; Shell mode
   ((string= major-mode "sh-mode")
    (let* ((pattern (thing-at-point 'symbol))
	   (search-list '(1 8))
	   (index (man-search-index pattern search-list)))
      (when index
	(man (format "%s (%d)" pattern index)))))
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

;; meson mode
(require 'meson-mode)

;; bitbake mode
(require 'bb-mode)
(setq auto-mode-alist (append '(("\\.bb\\'" . bb-mode)
				("\\.inc\\'" . bb-mode)
				("\\.bbclass\\'" . bb-mode)
				("\\.bbappend\\'" . bb-mode))
			      auto-mode-alist))

(provide 'my-programming)
