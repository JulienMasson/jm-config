;;; echat-bitlbee.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2020-03-31

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

(require 'seq)

;;; Customization

(defcustom echat-bitlbee-port-range (list 6670 6690)
  "Specify the range of which port bitlbee can use."
  :group 'echat)

;;; Class

(defclass echat-bitlbee ()
  ((name    :initarg :name    :type string)
   (port    :initarg :port    :type string)
   (args    :initarg :args    :initform nil)
   (buffer  :initarg :buffer  :initform nil)))

;;; Internal Functions

(defun echat-bitlbee--ports-unavailable ()
  (let (ports)
    (dolist (echat echats)
      (when (slot-exists-p echat 'bitlbee)
	(let ((bitlbee (oref echat bitlbee)))
	  (add-to-list 'ports (oref bitlbee port)))))
    ports))

(defun echat-bitlbee--process-sentinel (process status)
  )

(defun echat-bitlbee--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

;;; External Functions

(cl-defmethod echat-do-start ((bitlbee echat-bitlbee))
  (with-slots (name port args) bitlbee
    (when-let* ((default-directory "/")
		(program (executable-find "bitlbee"))
		(buffer-name (format "*Bitlbee: %s-%s*" name port))
		(buffer (get-buffer-create buffer-name))
		(process-name (format "bitlbee-%s-%s" name port))
		(program-args (append args (list "-p" port))))
      (oset bitlbee buffer buffer)
      (with-current-buffer buffer
	(insert (format "Run Bitlbee: %s %s\n\n" program
			(string-join program-args " "))))
      (when-let ((process (apply 'start-process process-name buffer
				 program program-args)))
	(set-process-filter process 'echat-bitlbee--process-filter)
	(set-process-sentinel process 'echat-bitlbee--process-sentinel)))))

(cl-defmethod echat-do-quit ((bitlbee echat-bitlbee))
  (when-let* ((buffer (oref bitlbee buffer))
	      (process (get-buffer-process buffer)))
    (when (zerop (process-exit-status process))
      (let ((kill-buffer-query-functions nil))
	(kill-buffer buffer)))))

(defun echat-bitlbee-create (name &rest args)
  (let* ((ports-range (apply #'number-sequence echat-bitlbee-port-range))
	 (ports-unavailable (echat-bitlbee--ports-unavailable))
	 (ports-available (cl-set-difference ports-range ports-unavailable))
	 (port (number-to-string (seq-random-elt ports-available))))
    (echat-bitlbee :name name :port port :args args)))

(provide 'echat-bitlbee)
