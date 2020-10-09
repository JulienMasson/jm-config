;;; keybindings-base.el --- Keybindings Base Configuration

;; Copyright (C) 2020 Julien Masson

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

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; disable suspend frame
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; movement windows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; movement right/left
(global-set-key (kbd "C-<right>") 'right-word)
(global-set-key (kbd "C-<left>") 'left-word)
(global-set-key (kbd "M-<right>") 'right-symbol)
(global-set-key (kbd "M-<left>") 'left-symbol)
(global-set-key (kbd "C-M-<right>") 'right-sexp)
(global-set-key (kbd "C-M-<left>") 'left-sexp)

;; tab bar right/left
(global-set-key (kbd "M-S-<right>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "M-S-<left>") 'tab-bar-switch-to-prev-tab)

;; return command
(global-set-key (kbd "C-<return>") 'jump-newline-and-indent)

;; kill commands
(global-set-key (kbd "C-w") 'kill-word-or-region)
(global-set-key (kbd "C-M-w") 'kill-sexp)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)

;; delete commands
(global-set-key (kbd "C-d") 'delete-char-or-region)
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "C-M-d") 'delete-sexp)
(global-set-key [C-delete] 'delete-word)
(global-set-key [C-backspace] 'backward-delete-word)

;; copy commands
(global-set-key (kbd "M-w") 'kill-ring-save-or-copy-line)
(global-set-key (kbd "C-c M-w") 'show-and-copy-buffer-filename)

;; isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(define-key isearch-mode-map (kbd "C-a") 'isearch-yank-beginning)
(define-key isearch-mode-map (kbd "C-e") 'isearch-yank-line)
(define-key isearch-mode-map (kbd "C-M-d") 'isearch-delete-selection)
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-kill-selection)
(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-symbol-or-char)
(define-key isearch-mode-map (kbd "M-w") 'isearch-kill-ring-save)

;; goto-line
(global-set-key (kbd "C-c l") 'goto-line)

;; rectangle commands
(global-set-key (kbd "C-c d") 'delete-rectangle)
(global-set-key (kbd "C-c M-i") 'string-rectangle)

;; revert buffer
(global-set-key (kbd "C-c M-r") 'revert-buffer)

;; comment region
(global-set-key (kbd "C-c m") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; window dedicated
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; dired
(global-set-key (kbd "C-c M-f") 'find-name-dired)
(define-key dired-mode-map (kbd "^")
  (lambda () (interactive) (find-alternate-file "..")))

;; help-mode
(require 'help-mode)
(define-key help-mode-map "n" 'help-go-forward)
(define-key help-mode-map "p" 'help-go-back)

(provide 'keybindings-base)
