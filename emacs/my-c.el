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

;; acscope
(require 'acscope)
(require 'acscope-kernel)
(setq acscope-find-auto-update nil)
(setq acscope-database-fast-symbol t)
(setq acscope-database-exclude-path (list ".ccls-cache"))
(acscope-global-setup)

;; global setting applied to specific c files
(defvar c-global-settings-list nil)

(defvar transient-default-log-arguments
  '((magit-log:magit-log-mode "--graph" "-n256" "--decorate")))

(defun apply-c-default-settings ()
  (setq transient-values (append transient-default-values
				 transient-default-log-arguments)))

(defun apply-c-global-settings (&rest _)
  (let* ((filename (expand-file-name default-directory))
	 (settings (seq-find (lambda (elem)
			       (string-match-p (car elem)
					       filename))
			     c-global-settings-list)))
    (if settings
	(funcall (cdr settings))
      (apply-c-default-settings))))

(add-hook 'c-mode-hook 'apply-c-global-settings)
(add-hook 'dired-mode-hook 'apply-c-global-settings)
(add-hook 'magit-status-mode-hook 'apply-c-global-settings)
(advice-add 'select-window :after #'apply-c-global-settings)

;; kernel global settings
(defvar transient-kernel-log-arguments
  '((magit-log:magit-log-mode "-n256" "--decorate")))

(defvar async-semantic-kernel-includes '("include/" "arch/arm64/include/"))

(defun cscope-kernel-source-file (dir)
  (funcall 'cscope-generate-from-objects dir))

(defun kernel-global-settings ()
  (setq transient-values (append transient-default-values
				 transient-kernel-log-arguments)))

(defun register-kernel-global-settings (path)
  (let ((kernel-includes (mapcar (lambda (p)
				   (concat path p))
				 async-semantic-kernel-includes)))
    (add-to-list 'magit-blacklist-repo path)
    (add-to-list 'c-global-settings-list `(,path . kernel-global-settings))))

;; checkpatch
(defun checkpatch ()
  (interactive)
  (let ((default-directory (magit-toplevel))
	(remote-head (magit-get-upstream-ref))
	(cmd "./scripts/checkpatch.pl")
	(cmd-options "--emacs"))
    (compile (format "%s %s --git %s..HEAD"
		     cmd cmd-options remote-head))))

;; set gnu makefile mode when opening defconfig file
(add-to-list 'auto-mode-alist '("_defconfig\\'" . makefile-gmake-mode))

;; device tree mode
(require 'dts-mode)

;; kconfig mode
(require 'kconfig-mode)

(provide 'my-c)
