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

;; ansi color
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; set default shell
(setq explicit-shell-file-name "/bin/bash")

;; bash as default over tramp
(setq tramp-connection-local-default-shell-variables
      '((shell-file-name . "/bin/bash")
        (shell-command-switch . "-c")))

(tramp-compat-funcall
 'connection-local-set-profile-variables
 'tramp-connection-local-default-shell-profile
 tramp-connection-local-default-shell-variables)

;; shell command completion
(require 'pcmpl-args)

;; use emacsclient as the $EDITOR
(require 'with-editor)
(add-hook 'shell-mode-hook 'with-editor-export-editor)

;; clear shell screen
(defun shell-clear ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

;; shell enter
(defun shell-press-ret ()
  (interactive)
  (if (get-buffer-process (current-buffer))
      (comint-send-input)
    (shell (current-buffer))))

;; export PS1
(defconst ps1-config "\\[\\033[01;38;5;33m\\]\\u  \\w${text}\n\\[\\033[01;38;5;42m\\]\\t \\033[0m")
(defun export-ps1 ()
  (when shell--start-prog
    (when-let ((process (get-buffer-process (current-buffer)))
	       (ps1 (shell-quote-argument ps1-config)))
      (goto-char (process-mark process))
      (process-send-string process (format " export PS1=%s\n" ps1))
      (while (accept-process-output process 0.1))
      (shell-clear))))
(setq shell-mode-hook (append shell-mode-hook (list 'export-ps1)))

;; completion history
(defun capf-comint-history ()
  (interactive)
  (unless (ring-empty-p comint-input-ring)
    (let ((target (field-string))
          candidates)
      (dotimes (index (ring-length comint-input-ring))
        (let ((entry (ring-ref comint-input-ring index)))
          (when (string-match target entry)
            (push entry candidates))))
      (completion-in-region (comint-line-beginning-position) (point)
                            `(lambda (string pred action)
                               (pcase action
                                 ('nil (try-completion string ,candidates))
                                 ('t (all-completions "" ',candidates pred))))))))

(provide 'my-shell)
