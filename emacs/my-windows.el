;;; my-windows.el --- Windows Configuration

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

;; vertical split by default
(setq split-height-threshold 119)
(setq split-width-threshold 143)

;; movement off the edge of the frame wraps around.
(setq windmove-wrap-around t)

;; shifted motion keys activate the mark momentarily
(setq shift-select-mode t)

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

;; enable ido-mode
(require 'ido)
(setq ido-ignore-files '("\\`\\.\\./" "\\`\\./"))
(ido-mode 1)
(ido-everywhere)

;; disable automatic file search in ido mode
(setq ido-auto-merge-work-directories-length -1)

;; ido-completing-read+
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; ido on yes-or-no
(defun ido-yes-or-no-p (prompt)
  (let* ((yes-or-no-prompt (concat prompt " "))
         (choices '("YES" "NO"))
         (answer (ido-completing-read yes-or-no-prompt choices
				      nil t nil nil)))
    (string= answer "YES")))

(defadvice yes-or-no-p (around use-ido activate)
  (setq ad-return-value (ido-yes-or-no-p prompt)))

;; hide welcome screen
(setq inhibit-startup-message t)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; show parenthese
(show-paren-mode 1)

;; load jm theme
(add-to-list 'custom-theme-load-path my-emacs-root-path)
(load-theme 'jm t)

;; highlight focus
(require 'face-remap)
(defvar highlight-focus-background "#303030")

(defun highlight-focus-swap (prev next)
  (when (and (buffer-live-p prev)
	     (not (eq prev next)))
    (with-current-buffer prev
      (setq face-remapping-alist nil)
      (force-mode-line-update)))
  (when (buffer-live-p next)
    (with-current-buffer next
      (face-remap-add-relative 'default :background
			       highlight-focus-background))))

(defun highlight-focus-check (old-fn &rest args)
  (let ((prev (current-buffer))
	next)
    (apply old-fn args)
    (setq next (current-buffer))
    (highlight-focus-swap prev next)))

(advice-add 'select-window :around #'highlight-focus-check)

;; line highlighting in all buffers
(require 'hl-line)
(global-hl-line-mode 1)

;; modeline
(require 'telephone-line)

(telephone-line-defsegment telephone-window-dedicated ()
  (when (window-dedicated-p)
    "Window Dedicated"))

(setq telephone-line-lhs
      '((accent . (telephone-line-airline-position-segment
		   telephone-window-dedicated))
        (nil    . (telephone-line-buffer-segment))))

(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment
		   telephone-line-process-segment))
        (accent . (telephone-line-major-mode-segment))))

(telephone-line-mode 1)

;; ansi color
(require 'comint)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

;; silent some message in echo area
(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'message #'silence))))

(dolist (func '(isearch-done
		undo
		basic-save-buffer
		push-mark))
  (if (functionp func)
      (advice-add func :around #'suppress-messages)))

(defun my-command-error-function (data context caller)
  (when (not (memq (car data) '(do-auto-save
				buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;; ansi color buffer
(defun ansi-color-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; window dedicated
(defun toggle-window-dedicated ()
  (interactive)
  (let (window (get-buffer-window (current-buffer)))
    (set-window-dedicated-p window (not (window-dedicated-p window)))))

;; toggle horizontal split
(defun toggle-horizontal-split ()
  (interactive)
  (if (not split-height-threshold)
      (setq split-height-threshold 119)
    (setq split-height-threshold nil)))

;; hide lines/region
(require 'hide-lines)
(require 'hide-region)

;; move lines
(require 'move-lines)
(move-lines-binding)

(provide 'my-windows)
