;;; my-gdb.el --- GDB Configuration

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

;; realgud
(require 'load-relative)
(require 'loc-changes)
(require 'realgud)
(setq realgud-safe-mode nil)

;; define own realgud file-find function
(defun my-realgud-file-find-function (marker filename directory &rest formats)
  (concat (f-root) filename))

(setq realgud-file-find-function 'my-realgud-file-find-function)

;; gdb get pid
(defun gdb-get-pid (process-name)
  (let* ((user (shell-command-to-string "echo -n $USER"))
	 (regexp (format "^%s\s*(\\d+).*%s$"
			 user process-name))
	 (cmd (format "ps aux | perl -ne 'print \"$1\" if /%s/'"
		      regexp)))
    (shell-command-to-string cmd)))

;; gdb
(defvar gdb-default-cmd "gdb")

(defun gdb (file)
  (interactive (list (ido-read-file-name "gdb on: ")))
  (realgud:gdb (concat "gdb " (untramp-path file))))

;; gdb attach
(defun gdb-attach (process)
  (interactive "sProcess Name: ")
  (let ((pid (gdb-get-pid process)))
    (when pid
      (realgud:gdb-pid (string-to-number pid)))))


;; kgdb
(defvar kgdb-default-port "ttyUSB0")
(defvar kgdb-default-speed 115200)
(defvar kgdb-default-vmlinux "vmlinux")

(defun kgdb-send-command (process cmd)
  (let ((str (concat cmd "\r")))
    (if (> (length str) 10)
	(mapc (lambda (c) (progn (comint-send-string process (string c))
				 (sleep-for 0.01))) (string-to-list str))
      (comint-send-string process str))))

(defun kgdb (vmlinux port speed trigger)
  (interactive (list (read-file-name "vmlinux: " nil kgdb-default-vmlinux t)
		     (read-file-name "Serial port: " "/dev" kgdb-default-port t)
		     (read-number "Serial speed: " kgdb-default-speed)
		     (yes-or-no-p "Trigger KGDB ? ")))
  (let ((default-directory (file-name-directory vmlinux))
	(kgdb-args (format "-ex \"target remote %s\"" port))
	serial-process)
    (when trigger
      (setq serial-process (make-serial-process :port port :speed speed))
      (when serial-process
	(kgdb-send-command serial-process "sudo su")
	(kgdb-send-command serial-process "echo g > /proc/sysrq-trigger")
	(delete-process serial-process)))
    (realgud:gdb (format "%s %s %s"
			 gdb-default-cmd
			 kgdb-args vmlinux))))


(provide 'my-gdb)
