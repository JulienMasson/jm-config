;;; my-system.el --- System Configuration

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

;; enable upcase/dowcase commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable narrowing commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; don't ask to follow symlink
(setq vc-follow-symlinks t)

;; remove eldoc mode
(global-eldoc-mode -1)

;; disable erase-buffer
(put 'erase-buffer 'disabled nil)

;; delete recursively without asking
(setq dired-recursive-deletes 'always)

;; save password
(setq password-cache-expiry nil)

;; dired Extra
(add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

;; default dired setting
(put 'dired-find-alternate-file 'disabled nil)

;; virtual desktop
(require 'virtual-desktops)
(setq virtual-desktops-display-mode-line nil)
(virtual-desktops-mode 1)

;; add 4 virtual desktop
(dotimes (i 4)
  (virtual-desktops-add 1))
(virtual-desktops-goto 1)

;; find file as root
(defun sudo-find-file (file)
  (interactive (list (read-file-name "Find file (as root): ")))
  (if (tramp-tramp-file-p file)
      (let* ((dissect (tramp-dissect-file-name file))
	     (method (tramp-file-name-method dissect))
	     (user (tramp-file-name-user dissect))
	     (host (tramp-file-name-host dissect))
	     (file-name (tramp-file-name-localname dissect)))
	(find-alternate-file (format "/%s:%s@%s|sudo:root@%s:%s"
				     method user host host file-name)))
    (find-alternate-file (concat "/sudo:root@localhost:" file))))

;; copy buffer filename
(defun show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; define key in help mode
(require 'help-mode)

;; tramp config
(require 'my-tramp)

;; org config
(require 'my-org)

;; dired config
(require 'my-dired)

;; erc config
(require 'my-erc)

;; programming config
(require 'my-programming)

;; shell config
(require 'my-shell)

;; git config
(require 'my-git)

;; pdf tools
(if (not (executable-find "epdfinfo"))
    (message "Please compile pdf-tools")
  (require 'pdf-tools)
  (require 'pdf-occur)
  (require 'pdf-history)
  (require 'pdf-links)
  (require 'pdf-outline)
  (require 'pdf-annot)
  (require 'pdf-sync)
  (pdf-tools-install))

;; pdfgrep
(if (not (executable-find "pdfgrep"))
    (message "Please install pdfgrep")
  (require 'pdfgrep)
  (pdfgrep-mode))

;; set default web browser
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;; mail config
(require 'my-mail)

;; utils
(require 'utils)
(require 'tail)
(require 'nmcli)
(require 'dmesg)
(require 'locate-database)
(require 'process)

;; Project manager
(require 'project-manager)
(require 'pm-emacslisp)
(register-project
 (make-project :name "Home"
	       :pm-backend "emacslisp"
	       :root-path "~"
	       :subprojects '(("private"        .       "/jm-config/emacs/modules/jm-private")
			      ("modules"	.       "/jm-config/emacs/modules")
			      ("emacs"		.	"/jm-config/emacs")
			      ("docs"		.	"/Documents/Docs")
			      ("gdrive"		.	"/Documents/gdrive"))))
(switch-project "Home")

;; Status
(require 'status)
(setq status-separator  " | ")
(status-add-to-left 'status-compilation)
(status-add-to-left 'status-cscope)
(status-add-to-left 'status-project-manager)
(status-add-to-left 'status-virtual-desktops)
(status-add-to-right 'status-erc)
(status-add-to-right 'status-notmuch)
(turn-on-status)

;; never request confirmation when opening large file
(setq large-file-warning-threshold nil)

;; silent url status
(require 'url-vars)
(setq url-show-status nil)

;; google translate
(require 'google-translate)

(defun translate-at-point ()
  (interactive)
  (let* ((text (if (use-region-p)
		   (buffer-substring-no-properties (region-beginning) (region-end))
		 (or (and (setq bounds (bounds-of-thing-at-point 'word))
			  (buffer-substring-no-properties (car bounds) (cdr bounds)))
		     (error "No word at point."))))
	 (detected-language (aref (google-translate-request "auto" "french" text) 2)))
    (cond
     ((string= detected-language "fr")
      (google-translate-translate "fr" "en" text))
     ((string= detected-language "en")
      (google-translate-translate "en" "fr" text)))))

;; add to list of directories to search for Info documentation files.
(add-to-list 'Info-default-directory-list "~/info")

;; default browser
(setq browse-url-browser-function 'browse-url-chrome)

;; interactive-align
(require 'ialign)

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


(provide 'my-system)
