;;; my-shell.el --- Shell Configuration

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

;; set default shell
(setq explicit-shell-file-name "/bin/bash")

;; ssh history
(defvar ssh-history '()
  "History of `ssh' function argument.")

(defun ssh (&optional login-host)
  "Create a new ssh shell on HOST."
  (interactive (list (read-string "login@host: " nil 'ssh-history)))
  (with-current-buffer (get-buffer-create (concat "*" login-host "*"))
    (setq default-directory (concat "/ssh:" login-host ":"))
    (shell (current-buffer))
    ;; Input ring bash history
    (set (make-local-variable 'comint-input-ring-file-name)
	 (concat "/ssh:" login-host ":~/.bash_history"))
    (comint-read-input-ring)))

;; clear shell screen
(defun my-clear ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(defun my-shell-hook ()
  (local-set-key (kbd "C-c l") 'my-clear))

(add-hook 'shell-mode-hook 'my-shell-hook)

;; history results
(defun search-history (pattern number)
  "Search in history"
  (interactive "sEnter pattern: \nnTail: ")
  (with-output-to-temp-buffer "*search-history-output*"
    (shell-command (format "cat ~/.zsh_history | grep %s | cut -c 16- | tail -n %d" pattern number)
                   "*search-history-output*"
                   "*Messages*")
    (pop-to-buffer "*search-history-output*")))

;; bash completion
(require 'bash-completion)
(bash-completion-setup)

;; multi term plus
(require 'multi-term-plus)
(setq multi-term-program explicit-shell-file-name)


(provide 'my-shell)
