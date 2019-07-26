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

(defvar jmail-update-sync-config nil)

;;; Internal Variables

(defconst jmail-update--buffer-name "*jmail-update*")

(defvar jmail-update--ongoing nil)

(defvar jmail-update--success-cb nil)

(defvar jmail-update--error-cb nil)

;;; Internal Functions

(defun jmail-update--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (mapconcat 'identity (split-string str "") "\n"))))

(defun jmail-update--reset-env ()
  (setq jmail-update--ongoing nil)
  (setq jmail-update--success-cb nil)
  (setq jmail-update--error-cb nil))

;; index
(defun jmail-update--index-process-sentinel (process status)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (jmail-process--funcall jmail-update--success-cb)
    (jmail-update--reset-env)))

(defun jmail-update--index ()
  (let* ((program (jmail-process--find-program default-directory "mu"))
	 (maildir (concat "--maildir=" jmail-top-maildir))
	 (args (list "index" "--nocolor" maildir))
	 (buffer (get-buffer jmail-update--buffer-name))
	 (process (apply 'start-file-process "jmail-update" buffer
			 program args)))
    (set-process-filter process 'jmail-update--process-filter)
    (set-process-sentinel process 'jmail-update--index-process-sentinel)))

;; sync
(defun jmail-update--sync-process-sentinel (process status)
  (if (and (eq (process-exit-status process) 0)
  	   (buffer-live-p (process-buffer process)))
      (jmail-update--index)
    (jmail-process--funcall jmail-update--error-cb)
    (jmail-update--reset-env)))

(defun jmail-update--get-sync-args ()
  (if jmail-update-sync-config
      (list "--all" "--config" jmail-update-sync-config)
    (list "--all")))

(defun jmail-update--sync ()
  (let* ((program (jmail-process--find-program default-directory
					       jmail-update-sync-cmd))
	 (args (jmail-update--get-sync-args))
	 (buffer (get-buffer-create jmail-update--buffer-name))
	 (process (apply 'start-file-process "jmail-update" buffer
			 program args)))
      (with-current-buffer buffer
	(erase-buffer))
      (set-process-filter process 'jmail-update--process-filter)
      (set-process-sentinel process 'jmail-update--sync-process-sentinel)))

;;; External Functions

(defun jmail-update (success error)
  (interactive)
  (unless jmail-update--ongoing
    (setq jmail-update--ongoing t)
    (setq jmail-update--success-cb success)
    (setq jmail-update--error-cb error)
    (jmail-update--sync)))

(provide 'jmail-update)
