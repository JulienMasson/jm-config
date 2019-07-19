;;; jmail-process.el --- XXXX

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

(cl-defstruct jprocess
  (dir     nil     :read-only t)
  (program nil     :read-only t)
  (args    nil     :read-only t)
  (cb      nil     :read-only t))

;;; Internal Variables

(defconst jmail-process--buffer-name "*jmail-process*")

(defvar jmail-process--next nil)

;;; Internal Functions

(defun jmail-process--funcall (func &rest args)
  (condition-case-unless-debug err
      (apply func args)
    (error (message "Error %s: %S" (symbol-name func) err))))

(defun jmail-process--tramp-executable-find (dir program-name)
  (with-parsed-tramp-file-name dir nil
    (let ((buffer (tramp-get-connection-buffer v))
	  (cmd (concat "which " program-name)))
      (with-current-buffer buffer
	(tramp-send-command v cmd)
	(goto-char (point-min))
	(when (looking-at "^\\(.+\\)")
	  (match-string 1))))))

(defun jmail-process--find-program (dir program-name)
  (let ((default-directory dir))
    (if (tramp-tramp-file-p dir)
	(jmail-process--tramp-executable-find dir program-name)
      (executable-find program-name))))

(defun jmail-process--extract-object (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^(" nil t)
      (backward-char)
      (when-let* ((end (ignore-errors (scan-sexps (point) 1)))
		  (str (buffer-substring (point) end)))
	(delete-region (point-min) end)
	(car (read-from-string str))))))

(defun jmail-process--process-objects (buffer)
  (let (object)
    (while (setq object (jmail-process--extract-object buffer))
      (jmail-process--funcall (jprocess-cb jmail-process--current) object))))

(defun jmail-process--process-sentinel (process status)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (jmail-process--process-objects (process-buffer process)))
  (setq jmail-process--current nil)
  (when jmail-process--next
    (jmail-process (pop jmail-process--next))))

(defun jmail-process--process-filter (process str)
  (unless (eq (process-status process) 'signal)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert str)
      (jmail-process--process-objects (current-buffer)))))

(defun jmail-process--run-later (jprocess)
  (add-to-list 'jmail-process--next jprocess t))

(defun jmail-process--run (jprocess)
  (when-let* ((dir (jprocess-dir jprocess))
	      (program-name (jprocess-program jprocess))
	      (args (jprocess-args jprocess))
	      (default-directory dir)
	      (program (jmail-process--find-program dir program-name))
	      (buffer (get-buffer-create jmail-process--buffer-name))
	      (process (apply 'start-file-process "jmail-process" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (setq jmail-process--current jprocess)
    (set-process-filter process 'jmail-process--process-filter)
    (set-process-sentinel process 'jmail-process--process-sentinel)))

(defun jmail-process--kill-if-running ()
  (when-let* ((process (get-buffer-process jmail-process--buffer-name))
	      (status (process-status process)))
    (when (eq status 'run)
      (interrupt-process process))))

;;; External Functions

(defun jmail-process-kill-all ()
  (setq jmail-process--next nil)
  (jmail-process--kill-if-running))

(defun jmail-process (jprocess)
    (if (jmail-process--kill-if-running)
	(jmail-process--run-later jprocess)
      (jmail-process--run jprocess)))

(provide 'jmail-process)
