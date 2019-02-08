;;; my-c.el --- C code Configuration

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

;; default indentation
(setq c-default-style '((c-mode . "linux") (other . "gnu")))

;; cscope
(require 'xcscope)
(setq cscope-option-use-inverted-index t)
(setq cscope-option-kernel-mode t)
(setq cscope-display-buffer-args nil)
(cscope-setup)

;; cscope regenerate files after indexing
(defvar cscope-files-cmd "find . -name \"*.[chxsS]\" > cscope.files")

(defun cscope-regenerate-cscope-files (top-directory)
  (shell-command cscope-files-cmd))
(advice-add 'cscope-index-files :after #'cscope-regenerate-cscope-files)

;; cscope search list
(defvar jm-cscope-search-list nil)
(defun cscope-add-cscope-search-list (dir)
  "Add cscope database to search list."
  (interactive "DAdd database: ")
  (add-to-list 'jm-cscope-search-list (list dir)))

(defun cscope-reset-cscope-search-list ()
  "Rest cscope search list"
  (interactive)
  (setq jm-cscope-search-list nil))

;; clang company
(require 'company-clang)
(add-to-list 'company-backends 'company-clang)

;; add extra args for clang based on cscope
(defvar cscope-include-dirs-cached nil)

(defun cscope-generate-include-dirs (dir)
  (unless (assoc dir cscope-include-dirs-cached)
    (let* ((default-directory dir)
	   (search-regex "\\.\\/(.*[i|I]nclude\\/).*h$")
	   (search-cmd (concat
			"cat cscope.files |"
			"perl -ne 'print \"$1\n\" if /"
			search-regex
			"/' |"
			"sort | uniq"))
	   (directories (split-string (shell-command-to-string
				       search-cmd))))
      (push dir directories)
      (add-to-list 'cscope-include-dirs-cached
		   `(,dir . ,(mapcar
			      (lambda (arg)
				(format "-I%s" (untramp-path
						(concat dir arg))))
			      directories)))))
  (assoc-default dir cscope-include-dirs-cached))

(defun extra-args-clang-company ()
  (when jm-cscope-search-list
      (setq-local company-clang-arguments
		  (apply 'append
			 (mapcar (lambda (arg)
				   (cscope-generate-include-dirs (car arg)))
				 jm-cscope-search-list)))))

(add-hook 'c-mode-hook 'extra-args-clang-company)

;; c header company
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;; ctags
(defun create-ctags-tag (dir)
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (shell-command "/usr/bin/ctags -e -R .")))


(provide 'my-c)
