;;; cscope-request.el --- Cscope Request Management

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

(require 'cl-macs)

(cl-defstruct cscope-request
  dir
  cmd
  (start 'ignore)
  (fail 'ignore)
  (finish 'ignore)
  (data nil))

;; Internal vars
(defvar cscope-program-name "cscope")
(defvar cscope-request-buffer "*cscope-process*")
(defvar cscope-requests nil)
(defvar cscope-current-request nil)
(defvar cscope-collect-data nil)

(defun cscope-find-program ()
  (executable-find cscope-program-name))

(defun cscope-data-list ()
  (if cscope-collect-data
      (delq nil (split-string cscope-collect-data "\n"))))

(defun cscope-next-request ()
  (setq cscope-current-request nil)
  (setq cscope-collect-data nil)
  (when-let ((request (pop cscope-requests)))
    (cscope-process-request request)))

(defun cscope-process-sentinel (process status)
  (let ((data (cscope-request-data cscope-current-request))
	(output (cscope-data-list))
	(func (if (eq (process-exit-status process) 0)
		  (cscope-request-finish cscope-current-request)
		(cscope-request-fail cscope-current-request))))
    (funcall func output status data)
    (cscope-next-request)))

(defun cscope-process-filter (process str)
  (setq cscope-collect-data (concat cscope-collect-data str)))

(defun cscope-start-process (dir program cmd)
  (let* ((default-directory dir)
	 (buffer (get-buffer-create cscope-request-buffer))
	 (process (apply 'start-file-process "cscope" buffer
			 program  cmd)))
    (set-process-filter process 'cscope-process-filter)
    (set-process-sentinel process 'cscope-process-sentinel)))

(defun cscope-raise-error (status)
  (let ((data (cscope-request-data cscope-current-request))
	(func (cscope-request-fail cscope-current-request)))
    (funcall func nil status data)
    (cscope-next-request)))

(defun cscope-run-command (request)
  (funcall (cscope-request-start request)
	   (cscope-request-data request))
  (let ((dir (cscope-request-dir request))
	(program (cscope-find-program))
	(cmd (cscope-request-cmd request)))
    (cond ((not (file-exists-p dir))
	   (cscope-raise-error (concat dir " doesn't exist !")))
	  ((string= "" program)
	   (cscope-raise-error (concat "Cannot find: " cscope-program-name)))
	  (t (cscope-start-process dir program cmd)))))

(defun cscope-cancel-current-request ()
  (interactive)
  (setq cscope-collect-data nil)
  (if-let ((process (get-buffer-process cscope-request-buffer)))
      (kill-process process)
    (setq cscope-current-request nil)))

(defun cscope-cancel-all-requests ()
  (interactive)
  (setq cscope-requests nil)
  (cscope-cancel-current-request))

(defun cscope-process-request (request)
  (if cscope-current-request
      (setq cscope-requests (append cscope-requests (list request)))
    (setq cscope-current-request request)
    (setq cscope-collect-data nil)
    (cscope-run-command request)))

(provide 'cscope-request)
