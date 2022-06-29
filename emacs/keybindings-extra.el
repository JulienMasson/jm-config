;;; keybindings-base.el --- Keybindings Extra Configuration

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

(require 'sgml-mode)

;; open browser
(global-set-key (kbd "C-c C-o") 'browse-url)

;; browse kill ring
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

;; hide lines
(global-set-key (kbd "C-c h") 'hide-lines)

;; visual-regexp
(global-set-key (kbd "C-\\") 'vr/query-replace)

;; interactive-align
(global-set-key (kbd "C-.") 'align)
(global-set-key (kbd "C-'") 'ialign)

;; surround
(global-set-key (kbd "C-;") 'emacs-surround)

;; shell mode
(define-key shell-mode-map (kbd "RET") #'shell-press-ret)
(define-key shell-mode-map (kbd "C-c l") 'shell-clear)
(define-key shell-mode-map (kbd "C-M-m") nil)
(define-key shell-mode-map (kbd "C-c SPC") nil)

;; shell command with editor
(global-set-key (kbd "M-&") 'with-editor-async-shell-command)
(global-set-key (kbd "M-!") 'with-editor-shell-command)

;; term
(define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
(define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)

;; magit
(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(global-set-key (kbd "C-c g c") 'my-magit-show-commit)
(global-set-key (kbd "C-c g d") 'magit-log-directory)
(global-set-key (kbd "C-c g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g l") 'magit-log-head)
(global-set-key (kbd "C-c g r") 'magit-reset-hard-head)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g t") 'magit-toggle-buffer-lock)
(global-set-key (kbd "C-c g y") 'magit-show-refs)
(define-key magit-mode-map [remap magit-copy-buffer-thing-as-kill] 'kill-ring-save)
(define-key magit-mode-map [remap magit-copy-buffer-revision] 'kill-ring-save)

;; forge
(define-key forge-topic-mode-map "g" 'forge-pull-current-topic)
(define-key magit-diff-mode-map (kbd "C-M-n") 'forge-next-diff-post)
(define-key magit-diff-mode-map (kbd "C-M-p") 'forge-previous-diff-post)

;; compilation
(define-key compilation-mode-map "e" 'compilation-send-command)
(global-set-key (kbd "C-c SPC") 'recompile)

;; manual at point
(global-set-key (kbd "M-h") 'manual-at-point)

;; acscope
(define-key acscope-mode-map (kbd "C-c s k") 'acscope-database-add-kernel)

;; realgud
(global-set-key (kbd "C-c r a") (lambda () (interactive) (realgud:cmdbuf-associate)))
(global-set-key (kbd "C-c r f") 'realgud-current-frame)
(global-set-key (kbd "C-c r c") 'realgud-calling-frame)

;; jmail
(global-set-key (kbd "C-M-m") 'jmail)
(global-set-key (kbd "C-x m") 'jmail-compose)

;; org
(global-set-key (kbd "C-c o a") 'jm-org-agenda)
(define-key org-agenda-mode-map "N" 'org-agenda-next-view)
(define-key org-agenda-mode-map "P" 'org-agenda-previous-view)
(define-key org-mode-map (kbd "C-c p") 'org-gtasks-push-current)
(define-key org-mode-map (kbd "C-c P") 'org-gtasks-pull-current)

;; ;; echat
;; (global-set-key (kbd "C-c e r") 'echat-init)
;; (global-set-key (kbd "C-c e q") 'echat-exit)
;; (global-set-key (kbd "C-c e C") 'echat-connect)
;; (global-set-key (kbd "C-c e D") 'echat-disconnect)
;; (global-set-key (kbd "C-c e i") 'echat-im)
;; (global-set-key (kbd "C-c e c") 'echat-channel)
;; (global-set-key (kbd "C-c e j") 'echat-jump)
;; (global-set-key (kbd "C-c e m") 'echat-mute-toggle)
;; (global-set-key (kbd "C-c e u") 'echat-unread)
;; (define-key lui-mode-map (kbd "C-M-u") 'echat-buffer-goto-unread-messages)

;; ;; lui
;; (define-key lui-mode-map (kbd "C-c C-e") 'jm-emojify-insert-emoji)

;; project manager
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c i m") 'project-search)
(global-set-key (kbd "C-c i d") 'project-debug)
(global-set-key (kbd "C-c i e") 'project-exec)
(global-set-key (kbd "C-c f") 'project-find-file)

;; html
(define-key html-mode-map (kbd "C-c <left>") nil)
(define-key html-mode-map (kbd "C-c <right>") nil)

;; conf
;; (provide 'conf-mode)
;; (define-key conf-mode-map [remap conf-space-keywords] 'recompile)

;; markdown
(define-key markdown-mode-map (kbd "C-c <left>")  nil)
(define-key markdown-mode-map (kbd "C-c <right>") nil)
(define-key markdown-mode-map (kbd "C-c <up>")    nil)
(define-key markdown-mode-map (kbd "C-c <down>")  nil)

(provide 'keybindings-extra)
