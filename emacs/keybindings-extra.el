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

;; occur
(global-set-key (kbd "C-M-o") 'occur-at-point)

;; search
(global-set-key (kbd "C-c M-s") 'my-search)
(global-set-key (kbd "C-M-s") 'my-search-at-point)

;; grep
(global-set-key (kbd "C-M-g") 'grep-at-point)
(define-key grep-mode-map "s" 'grep-save-buffer)
(define-key grep-mode-map (kbd "TAB") #'my-grep-context)
(define-key grep-mode-map "N" 'grep-command-next)
(define-key grep-mode-map "P" 'grep-command-previous)
(define-key grep-mode-map "e" 'grep-command-edit)
(define-key grep-mode-map "d" 'grep-command-change-directory)

;; dired
(global-set-key (kbd "C-c M-f") 'fd-find-name-dired)
(define-key dired-mode-map "=" 'dired-diff-files)
(define-key dired-mode-map "r" 'dired-diff-directories)
(define-key dired-mode-map "h" 'dired-do-hexl-find-file)
(define-key dired-mode-map "L" 'locate-dired)
(define-key dired-mode-map "U" 'unmark-all-dired-buffer)
(define-key dired-mode-map "K" 'kill-all-dired-buffer)
(define-key dired-mode-map "S" 'dired-sudo)

;; surround
(global-set-key (kbd "C-;") 'emacs-surround)

;; browse kill ring
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

;; markdown
(define-key markdown-mode-map (kbd "C-c <left>")  nil)
(define-key markdown-mode-map (kbd "C-c <right>") nil)
(define-key markdown-mode-map (kbd "C-c <up>")    nil)
(define-key markdown-mode-map (kbd "C-c <down>")  nil)

;; visual-regexp
(global-set-key (kbd "C-\\") 'vr/query-replace)

;; interactive-align
(global-set-key (kbd "C-.") 'align)
(global-set-key (kbd "C-'") 'ialign)

;; org
(global-set-key (kbd "C-c o a") 'jm-org-agenda)
(define-key org-agenda-mode-map "N" 'org-agenda-next-view)
(define-key org-agenda-mode-map "P" 'org-agenda-previous-view)
(define-key org-mode-map (kbd "C-c p") 'org-gtasks-push-current)
(define-key org-mode-map (kbd "C-c P") 'org-gtasks-pull-current)

;; open browser
(global-set-key (kbd "C-c C-o") 'browse-url)

;; magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-head)
(global-set-key (kbd "C-c g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g d") 'magit-log-directory)
(global-set-key (kbd "C-c g r") 'magit-reset-hard-head)
(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(global-set-key (kbd "C-c g y") 'magit-show-refs)
(global-set-key (kbd "C-c g c") 'magit-show-commit)
(define-key magit-mode-map [remap magit-copy-buffer-thing-as-kill] 'kill-ring-save)
(define-key magit-mode-map [remap magit-copy-buffer-revision] 'kill-ring-save)

;; forge
(define-key forge-topic-mode-map "g" 'forge-pull-current-topic)

;; compilation
(define-key compilation-mode-map "e" 'compilation-send-command)
(global-set-key (kbd "C-c SPC") (lambda () (interactive) (with-current-buffer "*compilation*" (recompile))))

;; refresh status
(global-set-key (kbd "C-c C-u") (lambda () (interactive) (run-at-time 1 status-refresh-timer-delay 'status-update)))

;; epurple
(global-set-key (kbd "C-c e c") 'epurple-chat)
(global-set-key (kbd "C-c e r") 'epurple-init)
(global-set-key (kbd "C-c e q") 'epurple-exit)
(global-set-key (kbd "C-c e j") 'epurple-jump)
(global-set-key (kbd "C-c e i") 'epurple-im)
(global-set-key (kbd "C-c e m") 'epurple-mute-toggle)
(global-set-key (kbd "C-c e u") 'epurple-unread)
(global-set-key (kbd "C-c e C") 'epurple-connect)
(global-set-key (kbd "C-c e D") 'epurple-disconnect)
(global-set-key (kbd "C-c e R") 'epurple-restart)
(global-set-key (kbd "C-c e U") 'epurple-update-restart)
(define-key lui-mode-map (kbd "C-M-u") 'epurple-buffer-goto-unread-messages)


;; lui
(define-key lui-mode-map (kbd "C-c C-e") 'jm-emojify-insert-emoji)

;; acscope
(define-key acscope-mode-map (kbd "C-c s k") 'acscope-database-add-kernel)

;; shell mode
(define-key shell-mode-map (kbd "TAB") #'company-manual-begin)
(define-key shell-mode-map (kbd "RET") #'shell-press-ret)

;; term
(define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
(define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)

;; manual at point
(global-set-key (kbd "M-h") 'manual-at-point)

;; jmail
(global-set-key (kbd "C-M-m") 'jmail)
(global-set-key (kbd "C-x m") 'jmail-compose)

;; project manager
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c i m") 'project-search)
(global-set-key (kbd "C-c i d") 'project-debug)
(global-set-key (kbd "C-c i e") 'project-exec)
(global-set-key (kbd "C-c f") 'project-find-file)

;; hide lines
(global-set-key (kbd "C-c h") 'hide-lines)

;; shell command with editor
(global-set-key (kbd "M-&") 'with-editor-async-shell-command)
(global-set-key (kbd "M-!") 'with-editor-shell-command)

;; realgud
(global-set-key (kbd "C-c r a") (lambda () (interactive) (realgud:cmdbuf-associate)))
(global-set-key (kbd "C-c r f") 'realgud-current-frame)
(global-set-key (kbd "C-c r c") 'realgud-calling-frame)

;; shell
(define-key shell-mode-map (kbd "C-c l") 'shell-clear)
(define-key shell-mode-map (kbd "C-M-m") nil)
(define-key shell-mode-map (kbd "C-c SPC") nil)

;; eaf
(global-set-key (kbd "C-M-w") 'eaf-open-browser)

;; html
(define-key html-mode-map (kbd "C-c <left>") nil)
(define-key html-mode-map (kbd "C-c <right>") nil)

(provide 'keybindings-extra)
