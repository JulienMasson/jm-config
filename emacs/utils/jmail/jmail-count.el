;;; jmail-count.el --- XXXX

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

(require 'cl-macs)

(cl-defstruct jcount
  (query nil :read-only t)
  (cb    nil :read-only t)
  (data  nil :read-only t))

;;; Internal Variables

(defconst jmail-count--buffer-name "*jmail-count*")

(defvar jmail-count--queues nil)

(defvar jmail-count--current nil)

;;; Internal Functions

(defun jmail-count--get-args (query)
  (list "find" "--fields" " " query))

(defun jmail-count--call-cb (count)
  (when-let ((cb (jcount-cb jmail-count--current))
	     (data (jcount-data jmail-count--current)))
    (jmail-funcall cb count data)))

(defun jmail-count--process-sentinel (process status)
  (when (eq (process-status process) 'exit)
    (if (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
	(with-current-buffer (process-buffer process)
	  (jmail-count--call-cb (- (line-number-at-pos (point-max)) 1)))
      (jmail-count--call-cb 0)))
  (if jmail-count--queues
      (jmail-count--process (pop jmail-count--queues))
    (setq jmail-count--current nil)
    (kill-buffer jmail-count--buffer-name)))

(defun jmail-count--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

(defun jmail-count--process (jcount)
  (when-let* ((program (jmail-find-program jmail-index-program))
	      (query (jcount-query jcount))
	      (args (jmail-count--get-args query))
	      (buffer (get-buffer-create jmail-count--buffer-name))
	      (process (apply 'start-file-process "jmail-count" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (setq jmail-count--current jcount)
    (set-process-filter process 'jmail-count--process-filter)
    (set-process-sentinel process 'jmail-count--process-sentinel)))

;;; External Functions

(defun jmail-count-quit ()
  (setq jmail-count--queues nil)
  (jmail-terminate-process-buffer jmail-count--buffer-name))

(defun jmail-count-get (query cb data)
  (let ((jcount (make-jcount :query query
			     :cb cb
			     :data data)))
    (if jmail-count--current
	(add-to-list 'jmail-count--queues jcount t)
      (jmail-count--process jcount))))

(provide 'jmail-count)
