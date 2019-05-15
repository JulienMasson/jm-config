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
(acscope-global-setup)

;; generate cscope files from objects files path
(defvar cscope-objects-excluded '("vmlinux.o" ".tmp_kallsyms*"))

(defun cscope-objects-list (dir)
  (let* ((cmd "find . -name \"*.o\"")
	 (str (shell-command-to-string cmd))
	 (objects (split-string str "\n"))
	 (regexp (format "\\(%s\\)"
			 (mapconcat 'identity cscope-objects-excluded
				    "\\|"))))
    (cl-remove-if (lambda (object)
		    (string-match-p regexp object))
		  objects)))

(defun cscope-get-dirs-from-objects (objects)
  (let (dirs)
    (mapc (lambda (object)
	    (add-to-list 'dirs (file-name-directory object) t))
	  objects)
    dirs))

(defun cscope-headers-list (dirs)
  (let (headers)
    (mapc (lambda (dir)
	    (let* ((cmd (format "find %s -name \"*.h\"" dir))
		   (str (shell-command-to-string cmd))
		   (results (split-string str "\n")))
	      (setq headers (append results headers))))
	  dirs)
    headers))

(defun cscope-default-headers-list ()
  (let* ((cmd "find ./include -name \"*.h\"")
	 (str (shell-command-to-string cmd)))
    (split-string str "\n")))

(defun cscope-generate-from-objects (dir)
  (interactive "DAdd database from objects: ")
  (let* ((objects (cscope-objects-list dir))
	 (dirs (cscope-get-dirs-from-objects objects))
	 (headers (cscope-headers-list dirs))
	 (default-headers (cscope-default-headers-list)))
    (with-current-buffer (find-file-noselect acscope-database-source-file)
      (erase-buffer)
      (mapc (lambda (object)
	      (insert (replace-regexp-in-string "\.o$" ".c" object))
	      (insert "\n"))
	    objects)
      (mapc (lambda (header)
	      (insert (concat header "\n")))
	    (append headers default-headers))
      (save-buffer))))

;; cscope custom index file
(defvar cscope-custom-source-file-cmds nil)

(defun cscope-custom-source-file-cmd (dir)
  (if-let ((func (assoc-default dir cscope-custom-source-file-cmds)))
      (funcall func dir)
    (funcall 'acscope-database-default-source-file-cmd dir)))

(setq acscope-database-source-file-cmd 'cscope-custom-source-file-cmd)

;; semantic
(require 'semantic)
(semantic-mode)

(defun semantic-set-include-path-from-toplevel (includes)
  (let* ((top (magit-toplevel))
	 (dirs (mapcar (lambda (dir) (concat top dir)) includes)))
    (setq-mode-local c-mode
		     semantic-dependency-system-include-path
		     (if (tramp-tramp-file-p top) nil dirs))))

;; company semantic
(require 'company-semantic)
(add-to-list 'company-backends 'company-semantic)

;; global setting applied to specific c files
(defvar c-global-settings-list nil)

(defun apply-c-default-settings ()
  (setq magit-log-arguments '("-n256" "--graph" "--decorate")))

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
(defun cscope-kernel-source-file (dir)
  (funcall 'cscope-generate-from-objects dir))

(defun kernel-global-settings ()
  (setq magit-log-arguments '("-n256" "--decorate"))
  (semantic-set-include-path-from-toplevel '("include/" "arch/arm64/include/")))

(defun register-kernel-global-settings (path)
  (add-to-list 'magit-blacklist-repo path)
  (add-to-list 'cscope-custom-source-file-cmds `(,path . cscope-kernel-source-file))
  (add-to-list 'c-global-settings-list `(,path . kernel-global-settings)))

;; ctags
(defun create-ctags-tag (dir)
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (shell-command "/usr/bin/ctags -e -R .")))

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
