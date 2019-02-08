;;; process.el --- Process Utils

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

(require 'utils)

(defgroup jm-process nil
  "JM Process group."
  :group 'convenience)

(define-derived-mode jm-process-mode fundamental-mode
  "jm-process"
  (local-set-key (kbd "q") 'jm-process-quit)
  (local-set-key (kbd "r") 'jm-process-restart)
  (local-set-key (kbd "d") 'jm-delete-process-content)
  (toggle-read-only t))

(defun jm-process-quit ()
  (interactive)
  (when (or (get-buffer-process (current-buffer))
	    (yes-or-no-p "Are you sure you want to kill this buffer ?"))
    (kill-buffer)))

(defun jm-delete-process-content ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to erase this buffer ?")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(defun jm-process-filter (process msg)
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
	  (light-save-excursion-if-not-at-point-max buffer
	    (goto-char (point-max))
	    (insert (ansi-color-apply msg))))))))

(defun jm-process-restart ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((process-buffer-name (buffer-name (current-buffer)))
           (cmd (if (string-match "\\*CMD: \\(.*\\)\\*" process-buffer-name)
                    (match-string 1 process-buffer-name))))
      (when cmd
        (process (start-process-shell-command
                    process-buffer-name
		    (current-buffer)
		    cmd))
        (set-process-filter process 'jm-process-filter)))))

(defun process (cmd)
  (interactive "Mcmd: ")
  (let ((process-buffer-name (format "*CMD: %s*" cmd)))
    (with-current-buffer (get-buffer-create process-buffer-name)
      (jm-process-mode)
      (let ((process (start-process-shell-command
                      process-buffer-name
		      (current-buffer)
		      cmd)))
        (set-process-filter process 'jm-process-filter))
      (pop-to-buffer-same-window (current-buffer)))))


(provide 'process)
