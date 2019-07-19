;;; jmail-menu.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-07-12

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;; External Variables

(defvar jmail-update-sync-cmd "mbsync")

(defvar jmail-update-sync-args (list "--all" "--config" "/home/julienm/jm-config/emacs/modules/jm-private/dotfiles/.mbsyncrc"))

;;; Internal Variables

(defconst jmail-update--buffer-name "*jmail-update*")

(defvar jmail-update--current-cb nil)

;;; Internal Functions

;; index
(defun jmail-update--index-process-sentinel (process status)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (when jmail-update--current-cb
      (jmail-process--funcall jmail-update--current-cb)
      (setq jmail-update--current-cb nil))))

(defun jmail-update--index-process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (mapconcat 'identity (split-string str "") "\n"))))

;; sync
(defun jmail-update--sync-process-sentinel (process status)
  (if (and (eq (process-exit-status process) 0)
  	   (buffer-live-p (process-buffer process)))
      (jmail-update-index)
    (setq jmail-update--current-cb nil)))

(defun jmail-update--sync-process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (mapconcat 'identity (split-string str "") "\n"))))

(defun jmail-update--sync ()
  (let* ((program (jmail-process--find-program default-directory jmail-update-sync-cmd))
	 (args jmail-update-sync-args)
	 (buffer (get-buffer-create jmail-update--buffer-name))
	 (process (apply 'start-file-process "jmail-update" buffer
			 program args)))
      (with-current-buffer buffer
	(erase-buffer))
      (set-process-filter process 'jmail-update--sync-process-filter)
      (set-process-sentinel process 'jmail-update--sync-process-sentinel)))

;;; External Functions

(defun jmail-update-index ()
  (let* ((program (jmail-process--find-program default-directory "mu"))
	 (args (list "index" "--nocolor" "--maildir=/home/julienm/Maildir/"))
	 (buffer (get-buffer jmail-update--buffer-name))
	 (process (apply 'start-file-process "jmail-update" buffer
			 program args)))
    (set-process-filter process 'jmail-update--index-process-filter)
    (set-process-sentinel process 'jmail-update--index-process-sentinel)))

(defun jmail-update (cb)
  (interactive)
  (unless jmail-update--current-cb
    (setq jmail-update--current-cb cb)
    (jmail-update--sync)))

(provide 'jmail-update)
