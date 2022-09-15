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

;; gdb get pid
(defun gdb-get-pid (process-name)
  (let* ((cmd (format "ps -C %s -o pid=" process-name))
	 (str (shell-command-to-string cmd)))
    (string-trim str)))

;; gdb
(defvar gdb-default-cmd "gdb")

(defun gdb (file &optional args env)
  (interactive (list (read-file-name "gdb on: ")))
  (let ((gdb-cmd (concat gdb-default-cmd " --args "))
	(gdb-args (delq nil (list (untramp-path file) args))))
    (gdb (concat gdb-cmd (string-join gdb-args)))))

;; gdb attach
(defun gdb-attach (process)
  (interactive "sProcess Name: ")
  (when-let ((pid (gdb-get-pid process)))
    (gdb (format "%s -p %s" gdb-default-cmd (string-to-number pid)))))

;; gdbserver
(defvar gdbserver-default-port 2345)

(defun gdb-get-host-ssh-config ()
  (delq nil (mapcar 'cadr (tramp-parse-sconfig "~/.ssh/config"))))

(defun gdbserver-send-command (host cmd)
  (let ((default-directory (format "/ssh:%s:/" host)))
    (replace-regexp-in-string
     "[[:blank:]]*\n$" ""
     (shell-command-to-string cmd))))

(defun gdbserver-send-command-async (host cmd)
  (let ((default-directory (format "/ssh:%s:/" host))
	(gdbserver-buffer "*gdbserver-buffer*")
	(kill-buffer-query-functions (remq 'process-kill-buffer-query-function
					   kill-buffer-query-functions)))
    (with-current-buffer (get-buffer-create gdbserver-buffer)
      (shell-command cmd (current-buffer))
      (kill-current-buffer))))

(defun gdbserver (host process)
  (interactive (let* ((host (completing-read "Host: "
					     (gdb-get-host-ssh-config)))
		      (process (read-file-name "Process name: "
					       (format "/ssh:%s:/" host))))
		 (list host process)))
  (let* ((ip-address (gdbserver-send-command host "hostname -I"))
	 (gdb-cmd (format "gdbserver %s:%s %s"
			  ip-address
			  gdbserver-default-port
			  (untramp-path process)))
	 (final-cmd (format "nohup %s &" gdb-cmd))
	 (gdb-args (format "-ex \"target remote tcp:%s:%s\""
			   ip-address gdbserver-default-port)))
    (gdbserver-send-command-async host final-cmd)
    (sleep-for 1)
    (gdb (format "%s %s" gdb-default-cmd gdb-args))))

(defun gdbserver-attach (host process)
  (interactive (list (completing-read "Host: " (gdb-get-host-ssh-config))
		     (read-string "Process name: ")))
  (let* ((process-pid (let ((default-directory (format "/ssh:%s:/" host)))
			(gdb-get-pid process)))
	 (ip-address (gdbserver-send-command host "hostname -I"))
	 (gdb-cmd (format "gdbserver --attach %s:%s %s"
			  ip-address
			  gdbserver-default-port
			  process-pid))
	 (final-cmd (format "nohup %s &" gdb-cmd))
	 (gdb-args (format "-ex \"target remote tcp:%s:%s\""
			   ip-address gdbserver-default-port)))
    (gdbserver-send-command-async host final-cmd)
    (sleep-for 1)
    (gdb (format "%s %s" gdb-default-cmd gdb-args))))

;; kgdb
(defvar kgdb-default-port "ttyUSB0")
(defvar kgdb-default-speed 115200)
(defvar kgdb-default-vmlinux "vmlinux")
(defvar kgdb-default-root-cmd "sudo su")

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
	(kgdb-send-command serial-process kgdb-default-root-cmd)
	(kgdb-send-command serial-process "echo g > /proc/sysrq-trigger")
	(delete-process serial-process)))
    (gdb (format "%s %s %s" gdb-default-cmd kgdb-args vmlinux))))

(provide 'my-gdb)
