;;; my-tramp.el --- Tramp Configuration

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

;; silent tramp message
(setq tramp-verbose 1)

;; expand tramp remote path
(require 'tramp-sh)
(add-to-list 'tramp-remote-path "~/.local/bin")

;; support executable-find over tramp
(defun tramp-executable-find (command)
  (with-parsed-tramp-file-name default-directory nil
    (let ((buffer (tramp-get-connection-buffer v))
	  (cmd (concat "which " command)))
      (with-current-buffer buffer
	(tramp-send-command v cmd)
	(goto-char (point-min))
	(when (looking-at "^\\(.*\\)")
	  (match-string 1))))))

(defun tramp-executable-find-around (old-fn &rest args)
  (if (tramp-tramp-file-p default-directory)
      (tramp-executable-find (car args))
    (apply old-fn args)))

(advice-add 'executable-find :around #'tramp-executable-find-around)

;; auto revert remote files
(setq auto-revert-remote-files t)

;; untramp path
(defun untramp-path (path)
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

(provide 'my-tramp)
