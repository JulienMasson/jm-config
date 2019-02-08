;;; tail.el --- Tail Utils

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

(require 'filenotify)

(defvar tail-buffer-name nil)
(defvar tail-buffer-file nil)

(defgroup tail nil
  "Tail group."
  :group 'convenience)

(define-derived-mode tail-mode fundamental-mode
  "tail"
  (local-set-key (kbd "q") 'tail-quit)
  (local-set-key (kbd "d") 'tail-delete-log-buffer)
  (toggle-read-only t))

(defun tail-quit ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to kill tail buffer ?")
    (kill-buffer)))

(defun tail-delete-log-buffer ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to erase this log buffer ?")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(defun tail-insert ()
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (insert-file-contents tail-buffer-file)
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun tail-notify (event)
  (with-current-buffer tail-buffer-name
    (tail-insert)
    (goto-char (point-max))))

(defun tail (file)
  (interactive (list (ido-read-file-name "File: ")))
  (setq tail-buffer-file file)
  (setq tail-buffer-name (concat "*TAIL-" file "*"))
  (with-current-buffer (get-buffer-create tail-buffer-name)
    (tail-mode)
    (tail-insert)
    (file-notify-add-watch file '(change attribute-change) 'tail-notify)
    (pop-to-buffer-same-window (current-buffer))))


(provide 'tail)
