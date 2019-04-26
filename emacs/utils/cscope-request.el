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

(defun cscope-process-sentinel (process str)
  (with-current-buffer (process-buffer process)
    (when (string= str "finished\n")
      (funcall (cscope-request-finish cscope-current-request)
	       (cscope-request-data cscope-current-request)
	       (cscope-data-list))
      (setq cscope-current-request nil)
      (setq cscope-collect-data nil)
      (when cscope-requests
	(cscope-process-request (pop cscope-requests))))))

(defun cscope-process-filter (process str)
  (setq cscope-collect-data (concat cscope-collect-data str)))

(defun cscope-run-command (request)
  (funcall (cscope-request-start request)
	   (cscope-request-data request))
  (let* ((default-directory (cscope-request-dir request))
	 (cmd (cscope-request-cmd request))
	 (buffer (get-buffer-create cscope-request-buffer))
	 (process (apply 'start-file-process "cscope" buffer
			 (cscope-find-program) cmd)))
    (set-process-filter process 'cscope-process-filter)
    (set-process-sentinel process 'cscope-process-sentinel)))

(defun cscope-cancel-requests ()
  (interactive)
  (when-let ((process (get-buffer-process cscope-request-buffer)))
    (kill-process process))
  (setq cscope-requests nil)
  (setq cscope-current-request nil)
  (setq cscope-collect-data nil))

(defun cscope-process-request (request)
  (let ((cmd (cscope-request-cmd request)))
    (if cscope-current-request
	(add-to-list 'cscope-requests request t)
      (setq cscope-current-request request)
      (setq cscope-collect-data nil)
      (cscope-run-command request))))

(provide 'cscope-request)
