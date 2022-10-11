;;; my-ui.el --- UI Configuration

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

;; no windows decorations
(set-frame-parameter nil 'undecorated t)

;; enable column number mode
(setq column-number-mode t)

;; disable menubar, toolbar and scrollbar
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

;; always keep the tab bar hidden
(setq tab-bar-show nil)

;; windows size
(setq default-frame-alist '((width . 80) (height . 40)))

;; set font size
(set-face-attribute 'default nil :height 100)

;; hide welcome screen
(setq inhibit-startup-message t)

;; show parenthese
(show-paren-mode 1)

;; no message when auto-saving
(setq auto-save-no-message t)

;; load jm faces
(require 'jm-faces)
(jm-faces-load)

;; doom modeline
(require 'doom-modeline)
(setq doom-modeline-height (frame-char-height))
(setq doom-modeline-buffer-file-name-style 'relative-from-project)

(doom-modeline-def-segment spaces
  "Spaces."
  (propertize "  " 'face (if (doom-modeline--active)
			     'mode-line
			   'mode-line-inactive)))

(doom-modeline-def-segment window-dedicated
  "Window Dedicated."
  (when (window-dedicated-p)
    (let ((face (if (doom-modeline--active)
		    '(:inherit error :box (:style released-button))
		  'mode-line-inactive)))
      (propertize " Window Dedicated " 'face face))))

(doom-modeline-def-segment jm-buffer-position
  "My buffer position information."
  (let* ((active (doom-modeline--active))
	 (percent (format-mode-line '("" mode-line-percent-position "%%")))
         (face (if active 'mode-line 'mode-line-inactive)))
    (concat (propertize percent 'face face)
	    (propertize "   " 'face (if active 'mode-line 'mode-line-inactive))
	    (propertize (format-mode-line "%l: %c") 'face face))))

(doom-modeline-def-modeline 'jm-modeline
  '(bar spaces jm-buffer-position spaces buffer-info)
  '(window-dedicated spaces misc-info spaces process spaces major-mode))

(doom-modeline-refresh-bars)
(doom-modeline-set-modeline 'jm-modeline 'default)

;; all the icons
(require 'all-the-icons)
(defvar jm-icons
 '((jmail-mode                all-the-icons-octicon  "mail"         :v-adjust 0.0)
   (jmail-compose-mode        all-the-icons-octicon  "pencil"       :v-adjust 0.0)
   (jmail-search-mode         all-the-icons-octicon  "mail"         :v-adjust 0.0)
   (acscope-buffer-mode       all-the-icons-octicon  "tools"        :v-adjust 0.0)
   (device-control-mode       all-the-icons-octicon  "tools"        :v-adjust 0.0)
   (fb-vpn-mode               all-the-icons-material "network_wifi" :v-adjust 0.0)
   (lt-mode                   all-the-icons-octicon  "tools"        :v-adjust 0.0)))
(setq all-the-icons-mode-icon-alist (append all-the-icons-mode-icon-alist jm-icons))

;; icons completion
(require 'all-the-icons-completion)
(all-the-icons-completion-mode)

;; icons dired
(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; icons ibuffer
(require 'all-the-icons-ibuffer)
(all-the-icons-ibuffer-mode 1)

;; highlight line
(require 'hl-line)
(global-hl-line-mode 1)

;; orderless
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))

;; vertico
(require 'vertico)
(require 'vertico-directory)
(setq vertico-cycle t)
(setq vertico-count-format nil)
(setq vertico-sort-function nil)
(vertico-mode)

;; vertico flat
(require 'vertico-flat)
(vertico-flat-mode)

;; marginalia
(require 'marginalia)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
(marginalia-mode)

;; corfu
(require 'corfu)
(setq corfu-auto t)
(global-corfu-mode)

;; kind icon
(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'my-ui)
