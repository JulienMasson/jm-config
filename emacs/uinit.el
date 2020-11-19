;;; uinit.el --- Micro Emacs Initialization File

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

;; my emacs root path
(defvar my-emacs-root-path (file-name-directory load-file-name))

;; disable menubar
(menu-bar-mode -1)

;; always keep the tab bar hidden
(setq tab-bar-show nil)

;; set font size
(set-face-attribute 'default nil :height 100)

;; enable ido-mode
(require 'ido)
(setq ido-ignore-files '("\\`\\.\\./" "\\`\\./"))
(ido-mode 1)
(ido-everywhere)

;; minimal dired config
(require 'dired)
(setq dired-recursive-deletes 'always)
(put 'dired-find-alternate-file 'disabled nil)

;; change default grep
(setq grep-command "grep --color -nsrH -E ")

;; disable automatic file search in ido mode
(setq ido-auto-merge-work-directories-length -1)

;; edition
(load-file (concat my-emacs-root-path "my-edit.el"))

;; faces
(load-file (concat my-emacs-root-path "utils/jm-faces.el"))
(jm-faces-load)

;; keybindings
(load-file (concat my-emacs-root-path "keybindings-base.el"))
(require 'keybindings-base)

;; clear echo area
(message nil)
