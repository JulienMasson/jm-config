;;; my-erc.el --- ERC Configuration

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

;; erc
(require 'erc)

;; erc notification
(add-to-list 'erc-modules 'notifications)

;; erc chat jump
(defun erc-chat-jump (buffer)
  (interactive (list (ido-completing-read "Switch to ERC buffer: "
					  (mapcar #'buffer-name (erc-chat-list))
					  nil t nil nil)))
  (switch-to-buffer buffer))

;; erc image
(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

;; emojify mode
(require 'emojify)
(add-hook 'erc-mode-hook 'emojify-mode)

;; exclude some types of messages
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"))

;; autojoin channel
(setq erc-autojoin-channels-alist '(("freenode.net" "#wayland" "#sway-devel" "#linux-amlogic")))

;; logging
(erc-log-mode)
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)
(setq erc-log-insert-log-on-open t)

;; disable erc-track-enable-keybindings
(setq erc-track-enable-keybindings nil)

;; apply face on log
(defvar erc-my-nickname-match "jmasson\\|julien")

(defun erc-log-match-face ()
  (list
   ;; own message line
   `(,(format "^\\(<\\)\\(%s\\)\\(>\\)[ \n]\\(.*\\)$" erc-my-nickname-match)
     (1 'erc-default-face)
     (2 'erc-my-nick-face)
     (3 'erc-default-face)
     (4 'erc-input-face))
   ;; standard message line
   `(,(format "^\\(<\\)\\(%s\\)\\(>\\)[ \n]\\(.*\\)$" erc-valid-nick-regexp)
     (1 'erc-default-face)
     (2 'erc-nick-default-face)
     (3 'erc-default-face)
     (4 'erc-default-face))
   ;; *** message line
   `("^\\(\\*\\*\\* .*\\)"
     (1 'erc-timestamp-face))
   ;; [date] message line
   `("^\\(\\[.*\\]\\)"
     (1 'erc-timestamp-face))))

(defun erc-apply-face-on-log ()
  (setq font-lock-defaults `(,(erc-log-match-face))))

(define-derived-mode erc-mode fundamental-mode "ERC"
  "Major mode for Emacs IRC."
  (erc-apply-face-on-log)
  (setq local-abbrev-table erc-mode-abbrev-table)
  (when (boundp 'next-line-add-newlines)
    (set (make-local-variable 'next-line-add-newlines) nil))
  (setq line-move-ignore-invisible t)
  (set (make-local-variable 'paragraph-separate)
       (concat "\C-l\\|\\(^" (regexp-quote (erc-prompt)) "\\)"))
  (set (make-local-variable 'paragraph-start)
       (concat "\\(" (regexp-quote (erc-prompt)) "\\)"))
  (setq-local completion-ignore-case t)
  (add-hook 'completion-at-point-functions 'erc-complete-word-at-point nil t))


(provide 'my-erc)
