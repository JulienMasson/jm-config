;;; dmesg.el --- Dmesg Utils

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

;; intel lib
(require 'utils)

;; global vars
(defvar dmesg-backends '(("local" . "while true; do sudo dmesg -c; sleep 1; done")
			 ("adb"   . "adb wait-for-device root && adb wait-for-device shell \"while true; do dmesg -c; sleep 1; done\"")))
(defvar dmesg-current-cmd nil)
(defvar dmesg-buffer-name "*DMESG*")
(defvar dmesg-faces `((".*\\(error\\|fail\\).*$"							.	error)
		      ("\\\[<[0-9a-f]+>\\\]"								.	warning)))
(defvar dmesg-regexp "\\(\\[\\s-*[[:digit:]]*\.[[:digit:]]*\\]\\)\\(.*\\)")

(defgroup dmesg nil
  "Dmesg group."
  :group 'convenience)

(define-derived-mode dmesg-mode fundamental-mode
  "dmesg"
  (local-set-key (kbd "q") 'dmesg-quit)
  (local-set-key (kbd "r") 'dmesg-restart)
  (local-set-key (kbd "d") 'dmesg-delete-log-buffer)
  (toggle-read-only t))

;; functions
(defun dmesg-insert (process timestamp data)
  "Insert TEXT in PROCESS buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
	  (light-save-excursion-if-not-at-point-max buffer
	    (goto-char (point-max))
	    (insert (concat timestamp data "\n"))))))))

(defun dmesg-process-filter (process msg)
  "Filter PROCESS output MSG."
  (dolist (msg-line (split-string msg "[\n\r]+"))
    (let ((timestamp (propertize
		      (replace-regexp-in-string dmesg-regexp "\\1" msg-line)
		      'face 'font-lock-comment-face))
	  (data (replace-regexp-in-string dmesg-regexp "\\2" msg-line)))
      (when (> (length data) 0)
	(dolist (f dmesg-faces)
	  (if (string-match (car f) data)
	      (setq data (propertize data 'face (cdr f)))))
	(dmesg-insert process timestamp data)))))

(defun dmesg-quit ()
  (interactive)
  (when (or (get-buffer-process (current-buffer))
	    (yes-or-no-p "Are you sure you want to kill dmesg buffer ?"))
    (kill-buffer)))

(defun dmesg-delete-log-buffer ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to erase this log buffer ?")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(defun dmesg-start-process ()
  (dmesg-mode)
  (let ((process (start-process-shell-command
		  "dmesg"
		  (current-buffer)
		  dmesg-current-cmd)))
    (set-process-filter process 'dmesg-process-filter)))

(defun dmesg-restart ()
  (interactive)
  (with-current-buffer (current-buffer)
    (dmesg-start-process)))

(defun dmesg (backend)
  "Start dmesg"
  (interactive (list (ido-completing-read "Backends: "
					  dmesg-backends
					  nil t nil nil)))
  (let ((dmesg-buffer-name (concat "*DMESG-" backend "*")))
    (setq dmesg-current-cmd (assoc-default backend dmesg-backends))
    (with-current-buffer (get-buffer-create dmesg-buffer-name)
      (dmesg-start-process)
      (pop-to-buffer-same-window (current-buffer)))))


(provide 'dmesg)
