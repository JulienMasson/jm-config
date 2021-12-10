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
(setq echat-nick-name "Julien Masson")

;; alert
(require 'alert)
(setq alert-log-messages nil)
(setq alert-hide-all-notifications t)
(setq alert-default-style nil)

;; circe images
(require 'circe-display-images)
(setq circe-display-images-image-regex
      "\\(https?:\/\/[^ ]*?\.\\\(?:png\\|jpg\\|jpeg\\|svg\\|gif\\).*\\)")
(add-hook 'lui-pre-output-hook 'circe-display-images)

;; emojify
(require 'emojify)
(add-hook 'lui-mode-hook #'emojify-mode)

(defvar jm-emojify-candidates '("😛" "😬" "😅" "🙂" "👍" "👌" "🤙" "😂" "😑" "😕""😤" "🙏" "💪" "💩"))

(defun jm-emojify-insert-emoji ()
  (interactive)
  (emojify-create-emojify-emojis)
  (let* ((emojify-minibuffer-reading-emojis-p t)
	 (minibuffer-setup-hook (cons #'emojify--completing-read-minibuffer-setup-hook
                                      minibuffer-setup-hook)))
    (insert (completing-read "Insert Emoji: " jm-emojify-candidates))))

;; lui
(require 'lui)
(setq lui-fill-column 90)
(setq lui-time-stamp-format "[%e-%b %H:%M]")
(setq lui-time-stamp-position nil)
(setq lui-fill-type nil)
(setq lui-flyspell-p t)
(setq lui-prompt-string circe-prompt-string)
(setq lui-scroll-behavior nil)

(provide 'my-chat)
