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
(require 'cl-seq)

(defgroup cscope-request nil
  "Cscope Request Management"
  :group 'tools)

(cl-defstruct cscope-request
  (dir    nil     :read-only t)
  (args   nil     :read-only t)
  (start  'ignore :read-only t)
  (fail   'ignore :read-only t)
  (finish 'ignore :read-only t)
  (data   nil     :read-only nil))

;;; Customization

(defcustom cscope-request-program-name "cscope"
  "Cscope program name"
  :type 'string
  :group 'cscope-request)

(defcustom cscope-request-buffer-name "*cscope-process*"
  "Cscope Request buffer name"
  :type 'string
  :group 'cscope-request)

;;; Internal Variables

(defvar cscope-request--pending nil
  "List of pending cscope request")

(defvar cscope-request--current nil
  "Current cscope request")

(defvar cscope-request--output nil
  "Data collected of the current cscope request")

;;; Internal Functions

(defun cscope-request--find-program (dir)
  "Find `cscope-request-program-name' executable"
  (let ((default-directory dir))
    (executable-find cscope-request-program-name)))

(defun cscope-request--get-output ()
  "Return a list of data collected from current cscope request"
  (if cscope-request--output
      (delq nil (split-string cscope-request--output "\n"))))

(defun cscope-request--process-next ()
  "Process next cscope request"
  (setq cscope-request--current nil)
  (when-let ((request (pop cscope-request--pending)))
    (cscope-request--exec request)))

(defun cscope-request--funcall (func &rest args)
  "Print error message when corresponding funcall failed"
  (condition-case-unless-debug err
      (apply func args)
    (error (message "Error %s: %S" (symbol-name func) err))))

(defun cscope-request--process-sentinel (process status)
  "Cscope request sentinel process"
  (let ((data (cscope-request-data cscope-request--current))
	(output (cscope-request--get-output))
    	(func (if (eq (process-exit-status process) 0)
    		  (cscope-request-finish cscope-request--current)
    		(cscope-request-fail cscope-request--current))))
    (cscope-request--funcall func output status data)
    (setq cscope-request--output nil)
    (cscope-request--process-next)))

(defun cscope-request--process-filter (process str)
  "Collect data from the current cscope request process"
  (setq cscope-request--output (concat cscope-request--output str)))

(defun cscope-request--start-process (dir program args)
  "Start the cscope request process"
  (let* ((default-directory dir)
	 (buffer (get-buffer-create cscope-request-buffer-name))
	 (process (apply 'start-file-process "cscope" buffer
			 program  args)))
    (set-process-filter process 'cscope-request--process-filter)
    (set-process-sentinel process 'cscope-request--process-sentinel)))

(defun cscope-request--raise-error (request status)
  "Raise an error and process next request"
  (let ((data (cscope-request-data request))
	(func (cscope-request-fail request)))
    (cscope-request--funcall func nil status data)
    (cscope-request--process-next)))

(defun cscope-request--exec (request)
  "Execute the cscope request"
  (setq cscope-request--current request)
  (cscope-request--funcall (cscope-request-start request)
			   (cscope-request-data request))
  (let* ((dir (cscope-request-dir request))
	 (program (cscope-request--find-program dir))
	 (args (cscope-request-args request)))
    (cond ((not (file-exists-p dir))
	   (cscope-request--raise-error
	    request (concat dir " doesn't exist !")))
	  ((string= "" program)
	   (cscope-request--raise-error
	    request (concat "Cannot find: " cscope-program-name)))
	  ((or (not (listp args)) (cl-member nil args))
	   (cscope-request--raise-error
	    request "Incorrect arguments format"))
	  (t (cscope-request--start-process dir program args)))))

;;; External Functions

(defun cscope-request-cancel-current ()
  "Cancel current cscope request"
  (interactive)
  (setq cscope-request--output nil)
  (if-let ((process (get-buffer-process cscope-request-buffer-name)))
      (kill-process process)
    (cscope-request--process-next)))

(defun cscope-request-cancel-all ()
  "Cancel all cscope request"
  (interactive)
  (setq cscope-request--pending nil)
  (cscope-request-cancel-current))

(defun cscope-request-run (request)
  "Run the cscope request if we don't have any pending cscope request.

Otherwise the cscope request is added to `cscope-request--pending' and will run later."
  (if cscope-request--current
      (setq cscope-request--pending (append cscope-request--pending
					    (list request)))
    (cscope-request--exec request)))

(provide 'cscope-request)
