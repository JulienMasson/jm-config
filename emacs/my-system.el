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

;; disable use of directory-local variables
(setq enable-dir-local-variables nil)

;; don't ask to follow symlink
(setq vc-follow-symlinks t)

;; remove eldoc mode
(global-eldoc-mode -1)

;; disable erase-buffer
(put 'erase-buffer 'disabled nil)

;; save password
(setq password-cache-expiry nil)

;; Avoid performance issues in files with very long lines.
(global-so-long-mode)

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

;; persistent scratch
(require 'persistent-scratch)
(persistent-scratch-setup-default)

;; browse kill ring
(require 'browse-kill-ring)

;; define key in help mode
(require 'help-mode)

;; tramp config
(require 'my-tramp)

;; org config
(require 'my-org)

;; dired config
(require 'my-dired)

;; chat config
(require 'my-chat)

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
(require 'process)

;; Project manager
(require 'project-manager)
(require 'pm-emacslisp)
(register-project
 (make-project :name "Home"
	       :pm-backend "emacslisp"
	       :root-path "~"
	       :subprojects `(("private" . ,(replace-regexp-in-string
					     (expand-file-name "~") ""
					     (concat my-emacs-root-path "modules/jm-private")))
			      ("modules" . ,(replace-regexp-in-string
					     (expand-file-name "~") ""
					     (concat my-emacs-root-path "modules")))
			      ("emacs"   . ,(replace-regexp-in-string
					     (expand-file-name "~") ""
					     my-emacs-root-path))
			      ("docs"    . "/Documents/Docs")
			      ("gdrive"  . "/Documents/gdrive"))))
(switch-project "Home")

;; Status
(require 'status)
(setq status-separator " | ")
(status-add-to-left 'status-compilation)
(status-add-to-left 'status-acscope)
(status-add-to-left 'status-project-manager)
(status-add-to-left 'status-tab-bar)
(status-add-to-right 'status-date)
(status-add-to-right 'status-epurple)
(status-add-to-right 'status-jmail)
(turn-on-status)

;; never request confirmation when opening large file
(setq large-file-warning-threshold nil)

;; silent url status
(require 'url-vars)
(setq url-show-status nil)

;; add to list of directories to search for Info documentation files.
(add-to-list 'Info-default-directory-list "~/info")

;; default browser
(setq browse-url-browser-function 'browse-url-chrome)

;; interactive-align
(require 'ialign)

;; edition
(require 'my-edit)

;; surround
(require 'emacs-surround)

(provide 'my-system)
