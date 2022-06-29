;;; init.el --- Emacs Initialization File

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

;; my private dotfiles path
(defvar my-private-dotfiles-path (concat my-emacs-root-path
					 "modules/jm-private/dotfiles/"))

;; add subdirs of emacs root path in load path
(add-to-list 'load-path my-emacs-root-path)
(let ((default-directory my-emacs-root-path))
  (normal-top-level-add-subdirs-to-load-path))

;; minimum config
(require 'my-ui)
(require 'my-system)
(require 'my-commands)
(require 'my-tramp)
(require 'my-dired)
(require 'my-grep)
(require 'my-search)
(require 'keybindings-base)

;; full config
(require 'my-modules)
(require 'keybindings-extra)
(require 'jm-private)
