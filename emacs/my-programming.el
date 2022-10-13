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

;; compilation buffer name
(defun compilation--generate-new-buffer-name (buffer name count)
  (let ((process (get-buffer-process buffer))
	(new-buffer (format "%s<%d>" name count)))
    (if (and process (eq (process-status process) 'run))
	(compilation--generate-new-buffer-name new-buffer name (cl-incf count))
      buffer)))

(defun compilation--new-buffer-name (name-of-mode)
  (let* ((buffer (format "*%s*" (downcase name-of-mode)))
	 (process (get-buffer-process buffer)))
    (if (and process (eq (process-status process) 'run))
	(if (yes-or-no-p "Reuse current buffer: ")
	    (when-let ((proc (get-buffer-process (current-buffer))))
	      (interrupt-process proc)
	      (sit-for 1)
	      (current-buffer))
	  (compilation--generate-new-buffer-name buffer buffer 1))
      buffer)))

(setq compilation-buffer-name-function #'compilation--new-buffer-name)

;; ansi color on compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; default indentation
(setq c-default-style '((c-mode    . "linux")
			(java-mode . "java")
			(other     . "gnu")))

;; spaces as default tabs
(setq-default indent-tabs-mode nil)

(defvar projects-indent-tab nil)

(defun project-set-indent-tab ()
  (let ((cur-dir (file-name-directory (buffer-file-name))))
    (catch 'found
      (dolist (project projects-indent-tab)
        (when (string-prefix-p project cur-dir)
          (setq indent-tabs-mode t)
          (throw 'found t))))))

(add-hook 'c-mode-hook #'project-set-indent-tab)

;; auto-detection indenting
(require 'dtrt-indent)
(dtrt-indent-mode t)

;; yaml
(require 'yaml-mode)

;; rust
(require 'rust-mode)

;; eglot
(require 'eglot)
(setq eglot-no-message t)
(setq eglot-sync-connect nil)
(setq eglot-autoshutdown t)
(setq eglot-stay-out-of '(flymake eldoc))

(defun eglot-shutdown-all ()
  (interactive)
  (when-let ((servers (cl-loop for servers
                               being hash-values of eglot--servers-by-project
                               append servers)))
    (mapc #'eglot-shutdown servers)))

(defun eglot-help-at-point ()
  (interactive)
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (when-let* ((blurb (and (not (seq-empty-p contents))
			    (eglot--hover-info contents range)))
		(hint (thing-at-point 'symbol))
		(buf-name (format "*eglot-help %s*" hint)))
      (unless (get-buffer buf-name)
	(with-current-buffer (get-buffer-create buf-name)
	  (insert blurb)
	  (help-mode)
	  (goto-char (point-min))))
      (pop-to-buffer buf-name))))

(defun eglot-help (hint)
  (interactive "sHelp: ")
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (when-let ((blurb (and (not (seq-empty-p contents))
			   (eglot--hover-info contents range)))
	       (buf-name (format "*eglot-help %s*" hint)))
      (unless (get-buffer buf-name)
	(with-current-buffer (get-buffer-create buf-name)
	  (insert blurb)
	  (help-mode)
	  (goto-char (point-min))))
      (pop-to-buffer buf-name))))

;; force ccls
(setcdr (assoc '(c++-mode c-mode) eglot-server-programs) '("ccls"))

;; eglot modes
(dolist (mode-hook (list 'c-mode-hook 'c++-mode-hook 'python-mode-hook
                         'sh-mode-hook 'rust-mode-hook))
  (add-hook mode-hook 'eglot-ensure))

;; acscope
(require 'acscope)
(require 'acscope-kernel)
(setq acscope-find-auto-update nil)
(setq acscope-database-fast-symbol t)
(setq acscope-database-exclude-path (list ".ccls-cache"))
(acscope-global-setup)

;; checkpatch
(defun checkpatch ()
  (interactive)
  (let ((default-directory (magit-toplevel))
	(remote-head (magit-get-upstream-ref))
	(cmd "./scripts/checkpatch.pl")
	(cmd-options "--emacs --strict"))
    (compile (format "%s %s --git %s..HEAD"
		     cmd cmd-options remote-head))))

;; set gnu makefile mode when opening defconfig file
(add-to-list 'auto-mode-alist '("_defconfig\\'" . makefile-gmake-mode))

;; hal to C++-mode
(add-to-list 'auto-mode-alist '("\\.hal\\'" . c++-mode))

;; device tree mode
(require 'dts-mode)

;; kconfig mode
(require 'kconfig-mode)

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
   ((derived-mode-p 'c-mode)
    (when-let* ((pattern (thing-at-point 'symbol))
	        (search-list '(2 3))
	        (index (man-search-index pattern search-list)))
      (man (format "%s (%d)" pattern index))))
   ;; Perl mode
   ((derived-mode-p 'cperl-mode)
    (cperl-perldoc-at-point))
   ;; Python/Rust/Shell mode
   ((derived-mode-p 'python-mode 'rust-mode 'sh-mode)
    (eglot-help-at-point))
   ;; Emacs lisp mode
   ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
    (let* ((string (thing-at-point 'symbol))
	   (symbol (intern-soft string)))
      (when symbol
	(cond ((fboundp symbol)
	       (describe-function symbol))
	      ((boundp symbol)
	       (describe-variable symbol))))))))

;; meson mode
(require 'meson-mode)

;; soong mode
(require 'soong-mode)

;; bitbake mode
(require 'bb-mode)
(setq auto-mode-alist (append '(("\\.bb\\'" . bb-mode)
				("\\.inc\\'" . bb-mode)
				("\\.bbclass\\'" . bb-mode)
				("\\.bbappend\\'" . bb-mode))
			      auto-mode-alist))

;; xref
(require 'xref)
(defun jm-xref--insert-xrefs (xref-alist)
  (let ((project (or (project-current) `(transient . ,default-directory))))
    (setq default-directory (project-root project))
    (insert (format "━▶ Project Root directory: %s\n"
		    (propertize (project-root project) 'face
				'font-lock-keyword-face)))
    (cl-loop for ((group . xrefs) . more1) on xref-alist do
	     (xref--insert-propertized '(face xref-file-header xref-group t)
                                       (format "\n*** %s:\n" (replace-regexp-in-string
							      (expand-file-name default-directory)
							      "" group)))
	     (cl-loop for (xref . more2) on xrefs do
                      (with-slots (summary location) xref
			(let* ((line (propertize (format "%d" (xref-location-line location))
						 'face 'xref-line-number))
                               (prefix (format "[%s]" line)))
                          (xref--insert-propertized
			   (list 'xref-item xref 'keymap xref--button-map)
			   (format "%-8s %s" prefix (string-trim summary)))))
		      (insert "\n")))))
(advice-add 'xref--insert-xrefs :override #'jm-xref--insert-xrefs)

;; generate compile_commands.json
(defun generate-compile-commands ()
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (async-shell-command "gen_compile_commands.py")))

;; gdb
;; (require 'my-gdb)

(provide 'my-programming)
