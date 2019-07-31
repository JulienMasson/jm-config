;;; jmail-attachment.el --- XXXX

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

;;; Internal Variables

(defconst jmail-attachment--buffer-name "*jmail-attachment*")

;;; Internal Functions

(defun jmail-attachment--process-sentinel (process status)
  (if (eq (process-exit-status process) 0)
    (with-current-buffer (process-buffer process)
      (let ((dir default-directory))
	(kill-this-buffer)
	(dired-other-window dir)
	(revert-buffer)))
    (switch-to-buffer-other-window (process-buffer process))))

(defun jmail-attachment--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

(defun jmail-attachment--process (path args)
  (when-let* ((default-directory path)
	      (program (jmail-find-program jmail-index-program))
	      (buffer (get-buffer-create jmail-attachment--buffer-name))
	      (process (apply 'start-file-process "jmail-attachment" buffer
			      program args)))
    (set-process-filter process 'jmail-attachment--process-filter)
    (set-process-sentinel process 'jmail-attachment--process-sentinel)))

(defun jmail-attachment--build-args (outdir msg-path args)
  (list "extract" "--overwrite"
	(concat "--target-dir=" outdir)
	args msg-path))

;;; External Functions

(defun jmail-attachment-save (msg-path index outdir)
  (let* ((parts (format "--parts=%d" index))
	 (args (jmail-attachment--build-args outdir msg-path parts)))
    (jmail-attachment--process outdir args)))

(defun jmail-attachment-save-all (msg-path outdir)
  (let* ((save-all "--save-attachments")
	 (args (jmail-attachment--build-args outdir msg-path save-all)))
    (jmail-attachment--process outdir args)))

(provide 'jmail-attachment)
