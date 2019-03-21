;;; my-keybindings.el --- Keybindings Configuration

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

;; kill commands
(global-set-key (kbd "C-w") 'kill-word-or-region)
(global-set-key (kbd "M-w") 'kill-ring-save)
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
(global-set-key (kbd "C-c M-w") 'show-and-copy-buffer-filename)

;; isearch
(define-key isearch-mode-map (kbd "C-a") 'isearch-yank-beginning)
(define-key isearch-mode-map (kbd "C-e") 'isearch-yank-line)
(define-key isearch-mode-map (kbd "C-M-d") 'isearch-delete-selection)
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-kill-selection)
(define-key isearch-mode-map (kbd "C-f") 'isearch-yank-symbol-or-char)

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

;; markdown
(define-key markdown-mode-map (kbd "C-c <left>")  'windmove-left)
(define-key markdown-mode-map (kbd "C-c <right>") 'windmove-right)
(define-key markdown-mode-map (kbd "C-c <up>")    'windmove-up)
(define-key markdown-mode-map (kbd "C-c <down>")  'windmove-down)

;; visual-regexp
(global-set-key (kbd "M-%") 'vr/query-replace)

;; interactive-align
(global-set-key (kbd "M-@") 'ialign)

;; goto virtual desktops
(global-set-key (kbd "M-1") (lambda () (interactive) (virtual-desktops-goto 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (virtual-desktops-goto 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (virtual-desktops-goto 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (virtual-desktops-goto 4)))

;; org
(global-set-key (kbd "C-c o a") 'jm-org-agenda)

;; magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-head)
(global-set-key (kbd "C-c g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g r") 'magit-reset-hard)
(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(define-key magit-mode-map [remap magit-copy-buffer-thing-as-kill] 'kill-ring-save)
(define-key magit-mode-map [remap magit-copy-buffer-revision] 'kill-ring-save)

;; compilation
(global-set-key (kbd "C-c SPC") (lambda () (interactive) (with-current-buffer "*compilation*" (recompile))))

;; open browser
(global-set-key (kbd "C-c C-o") 'browse-url)

;; refresh status
(global-set-key (kbd "C-c C-u") (lambda () (interactive) (run-at-time 1 status-refresh-timer-delay 'status-update)))

;; bitlbee
(global-set-key (kbd "C-c e b") 'bitlbee-update-all)
(global-set-key (kbd "C-c e c") 'bitlbee-chat)
(global-set-key (kbd "C-c e j") 'erc-chat-jump)

;; cscope
(define-key cscope-minor-mode-keymap  "\C-csa" 'cscope-add-cscope-search-list)
(define-key cscope-minor-mode-keymap  "\C-csr" 'cscope-reset-cscope-search-list)

;; map TAB to company completion in shell mode
(define-key shell-mode-map (kbd "TAB") #'company-manual-begin)

;; pycscope
(define-key cscope-minor-mode-keymap (kbd "C-c s p") 'cscope-pycscope)

;; manual at point
(global-set-key (kbd "M-h") 'manual-at-point)

;; mail
(global-set-key (kbd "C-x m") 'mail-compose-new)
(global-set-key (kbd "C-M-m") 'mail-client)

;; notmuch
(define-key notmuch-common-keymap "j" 'notmuch-jump-folder)
(define-key notmuch-common-keymap "s" 'notmuch-tree)
(define-key notmuch-common-keymap "u" 'notmuch-unread-at-point)
(define-key notmuch-common-keymap "U" 'notmuch-show-unread)
(define-key notmuch-common-keymap "z" 'notmuch-search)
(define-key notmuch-show-mode-map "n" 'my-notmuch-show-next-message)
(define-key notmuch-show-mode-map "p" 'my-notmuch-show-prev-message)
(define-key notmuch-tree-mode-map "f" 'notmuch-tree-toggle-folding-thread-all)
(define-key notmuch-tree-mode-map "m" 'notmuch-remove-unread-smart)
(define-key notmuch-tree-mode-map "M" 'notmuch-remove-unread-all)
(define-key notmuch-tree-mode-map (kbd "TAB") 'notmuch-tree-toggle-folding-thread)

;; project manager
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c f") 'project-find-file-subproject)

;; grep
(global-set-key (kbd "C-M-g") 'grep-at-point)
(define-key grep-mode-map "s" 'grep-save-buffer)
(define-key grep-mode-map (kbd "TAB") #'my-grep-context)

;; occur
(global-set-key (kbd "C-M-o") 'occur-at-point)

;; window dedicated
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; hide lines
(global-set-key (kbd "C-c h") 'hide-lines)

;; shell command with editor
(global-set-key (kbd "M-&") 'with-editor-async-shell-command)
(global-set-key (kbd "M-!") 'with-editor-shell-command)

;; translate at point
(global-set-key (kbd "M-*") 'translate-at-point)

;; dired
(global-set-key (kbd "C-c M-f") 'find-name-dired)
(define-key dired-mode-map "=" 'dired-diff-files)
(define-key dired-mode-map "r" 'dired-diff-directories)
(define-key dired-mode-map "h" 'dired-do-hexl-find-file)
(define-key dired-mode-map (kbd "^")
  (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map "h" 'dired-do-hexl-find-file)
(define-key dired-mode-map "L" 'locate-database)
(define-key dired-mode-map "U" 'unmark-all-dired-buffer)
(define-key dired-mode-map "K" 'kill-all-dired-buffer)

;; erc
(define-key erc-mode-map (kbd "C-c C-i") 'emojify-insert-emoji)

;; realgud
(global-set-key (kbd "C-c r a") (lambda () (interactive) (realgud:cmdbuf-associate)))
(global-set-key (kbd "C-c r f") 'realgud-current-frame)
(global-set-key (kbd "C-c r c") 'realgud-calling-frame)

;; help-mode
(define-key help-mode-map "n" 'help-go-forward)
(define-key help-mode-map "p" 'help-go-back)

;; shell
(define-key shell-mode-map (kbd "C-c l") 'shell-clear)
(define-key shell-mode-map (kbd "C-M-m") nil)
(define-key shell-mode-map (kbd "C-c SPC") nil)


(provide 'my-keybindings)
