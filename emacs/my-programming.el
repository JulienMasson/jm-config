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

;; visual regexp
(require 'visual-regexp)

;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; save backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; default indentation
(setq c-default-style '((c-mode    . "linux")
			(java-mode . "java")
			(other     . "gnu")))

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

;; eglot
(require 'eglot)
(setq eglot-no-message t)
(setq eglot-flymake-enable nil)
(setq eglot-eldoc-enable nil)
(setq eglot-sync-connect nil)
(setq eglot-autoshutdown t)

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

;; enable eglot
(defun eglot-ensure-current-program ()
  (assoc-default major-mode eglot-server-programs
		 (lambda (m1 m2)
		   (cl-find m2 (if (listp m1) m1 (list m1))
			    :test #'provided-mode-derived-p))))

(defun my-eglot-ensure ()
  (when-let* ((program (eglot-ensure-current-program))
	      (executable (executable-find (car program))))
    (unless (string= executable "")
      (eglot-ensure))))

(add-hook 'c-mode-hook 'my-eglot-ensure)
(add-hook 'python-mode-hook 'my-eglot-ensure)
(add-hook 'sh-mode-hook 'my-eglot-ensure)

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
	(cmd-options "--emacs"))
    (compile (format "%s %s --git %s..HEAD"
		     cmd cmd-options remote-head))))

;; set gnu makefile mode when opening defconfig file
(add-to-list 'auto-mode-alist '("_defconfig\\'" . makefile-gmake-mode))

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
    (eglot-help-at-point))
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

(provide 'my-programming)
