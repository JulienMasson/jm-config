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

;; emojify
(require 'emojify)

;; slack
(require 'slack)
(setq slack-buffer-emojify t)
(setq slack-log-level 'error)

;; circe
(require 'circe)
(setq circe-server-killed-confirmation nil)
(setq circe-channel-killed-confirmation nil)
(add-hook 'circe-chat-mode-hook #'emojify-mode)

;; circe images
(require 'circe-display-images)
(setq circe-display-images-image-regex
      "\\(https?:\/\/[^ ]*?\.\\\(?:png\\|jpg\\|jpeg\\|svg\\|gif\\).*\\)")
(enable-circe-display-images)

;; lui
(require 'lui)
(setq lui-fill-column 80)
(setq lui-time-stamp-format "[%e-%b %H:%M]")
(setq lui-time-stamp-position nil)
(setq lui-fill-type "")
(setq lui-flyspell-p t)
(setq lui-prompt-string circe-prompt-string)

;; lui logging
(require 'lui-logging)
(setq lui-logging-directory "~/.cache/lui-logs")
(enable-lui-logging-globally)

(provide 'my-chat)