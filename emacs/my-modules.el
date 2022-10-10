;;; my-modules.el --- Modules Configuration

;; Copyright (C) 2022 Julien Masson

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

;; browse kill ring
(require 'browse-kill-ring)

;; hide lines/region
(require 'hide-lines)
(require 'hide-region)

;; move lines
(require 'move-lines)
(move-lines-binding)

;; visual regexp
(require 'visual-regexp)

;; interactive-align
(require 'ialign)

;; surround
(require 'emacs-surround)

;; utils
(require 'utils)

;; shell config
(require 'my-shell)

;; git config
(require 'my-git)

;; programming config
(require 'my-programming)

;; mail config
(require 'my-mail)

;; org config
(require 'my-org)

;; chat config
;; (require 'my-chat)

;; Project manager
(require 'pm-simple)
(register-project
 (make-project :name "Home"
	       :pm-backend "simple"
	       :root-path "~/"
	       :subprojects '(("private" . (concat my-emacs-root-path "modules/jm-private/"))
			      ("modules" . (concat my-emacs-root-path "modules/"))
			      ("emacs"   . my-emacs-root-path)
			      ("gdrive"  . "Documents/gdrive/"))))
(switch-project "Home")

;; Status
(require 'status)
(setq status-separator " | ")

(require 'status-compilation)
(status-add-to-left 'status-compilation)

(require 'status-acscope)
(status-add-to-left 'status-acscope)

(require 'status-project-manager)
(status-add-to-left 'status-project-manager)

(require 'status-tab-bar)
(status-add-to-left 'status-tab-bar)

(require 'status-date)
(status-add-to-right 'status-date)

;; (require 'status-echat)
;; (status-add-to-right 'status-echat)

(require 'status-jmail)
(status-add-to-right 'status-jmail)

(require 'status-org-gcal)
(status-add-to-right 'status-org-gcal)

(turn-on-status)

;; pdf tools
;; (if (not (executable-find "epdfinfo"))
;;     (message "Please compile pdf-tools")
;;   (require 'pdf-tools)
;;   (require 'pdf-occur)
;;   (require 'pdf-history)
;;   (require 'pdf-links)
;;   (require 'pdf-outline)
;;   (require 'pdf-annot)
;;   (require 'pdf-sync)
;;   (pdf-tools-install))

;; pdfgrep
(if (not (executable-find "pdfgrep"))
    (message "Please install pdfgrep")
  (require 'pdfgrep)
  (pdfgrep-mode)
  (setq pdfgrep-options " --color=always -nrH "))

;; view large files
(require 'vlf-setup)

;; silent url status
(require 'url-vars)
(setq url-show-status nil)

;; persistent scratch
(require 'persistent-scratch)
(persistent-scratch-setup-default)

(provide 'my-modules)
