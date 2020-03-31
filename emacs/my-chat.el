;;; my-chat.el --- Chat Configuration

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

;; echat
(require 'echat)

;; slack show emoji
(setq slack-buffer-emojify t)

;; slack error log level
(setq slack-log-level 'error)

;; circle no confirmation
(setq circe-server-killed-confirmation nil)
(setq circe-channel-killed-confirmation nil)

;; erc
(require 'erc)

;; erc notification
(add-to-list 'erc-modules 'notifications)

;; erc chat jump
(defun erc-chat-buffer-name-unread ()
  (let* ((channels-buffer (mapcar #'car erc-modified-channels-alist))
	 (channels (seq-filter #'erc-not-blacklist-p
			       channels-buffer)))
    (mapcar #'buffer-name channels)))

(defun erc-chat-buffer-name ()
  (let ((all (mapcar #'buffer-name (erc-chat-list)))
	(unread (erc-chat-buffer-name-unread)))
    (delete-dups (append unread all))))

(defun erc-chat-jump (buffer)
  (interactive (list (ido-completing-read "Switch to ERC buffer: "
					  (erc-chat-buffer-name)
					  nil t nil nil)))
  (switch-to-buffer buffer))

;; erc image
(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

;; exclude some types of messages
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"))

;; autojoin channel
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs-sophia")))

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

(provide 'my-chat)
