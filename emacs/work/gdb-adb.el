;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               GDB WITH ADB                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; require
(require 'gdb-mi)

;; gdb settings
(setq gdb-show-main t)
(setq gdb-many-windows t)

;; default var
(defvar gdb-android-default-directory "/home/jmassonx/Documents/Intel/ndg-android")
(defvar gdb-android-cmd (concat gdb-android-default-directory "/prebuilts/gcc/linux-x86/x86/x86_64-linux-android-4.9/bin/x86_64-linux-android-gdb"))
(defvar gdb-client-cmds (concat gdb-android-default-directory "/out/target/product/grant/gdbclient.cmds"))
(defvar gdb-android-symbols (concat gdb-android-default-directory "/out/target/product/grant/symbols/"))
(defvar gdb-adb-script "/home/jmassonx/jm-config/emacs/work/gdb-adb")
(defvar gdb-adb-current-program nil)
(defvar gdb-adb-program-history nil)
(defvar gdb-adb-cmds
  '(("server"			.       gdb-adb-server)
    ("client"			.       gdb-adb-client)
    ("attach"		        .	gdb-adb-attach)))

(defun gdb-adb-server (program)
  (interactive (list (read-string "Program to debug: "
				  nil 'gdb-adb-program-history
				  (car gdb-adb-program-history))))
  (setq gdb-adb-current-program program)
  (async-shell-command (concat "adb forward tcp:50100 tcp:50100 && adb shell gdbserver :50100 " program)))

(defun gdb-adb-client ()
  (interactive)
  (if gdb-adb-current-program
      (let ((program-path (shell-command-to-string (concat "find " gdb-android-symbols " -type f -name " gdb-adb-current-program " | tr -d '\n'"))))
	(if program-path
	    (let ((default-directory gdb-android-default-directory))
	      ;; (setq gud-gdb-command-name (concat " " gdb-android-cmd " " gdb-client-cmds " " program-path))
	      ;; (gdb-adb gdb-adb-script gud-gdb-command-name))
	      (gdb-adb gdb-adb-script (concat gdb-android-cmd " " gdb-client-cmds " " program-path)))
	  (message "Program not found in symbols directory")))
    (message "GDB server not running or attached")))

(defun gdb-adb-attach (program)
  (interactive (list (read-string "Program to attach: "
				  nil 'gdb-adb-program-history
				  (car gdb-adb-program-history))))
  (setq program-pid (shell-command-to-string (concat "adb shell pgrep " program " | tr -d '\r\n'")))
  (if program-pid
      (progn
	(setq gdb-adb-current-program program)
	(async-shell-command (concat "adb forward tcp:50100 tcp:50100 && adb shell gdbserver :50100 --attach " program-pid)))
    (message "Process not found on device")))

;; Non stop Mode
;; (setq gdb-non-stop-setting t) ;; non stop mode
;; (setq gdb-gud-control-all-threads nil) ;; only the current thread is stopped/continued
;; or
;; (setq gdb-gud-control-all-threads t) ;; interruption and continuation commands apply to all threads

;; All stop Mode
;; (setq gdb-non-stop-setting nil) ;; all stop mode


(defun gdb-adb-tools (cmds)
    (interactive (list (ido-completing-read "GDB with adb: "
					  (mapcar 'car gdb-adb-cmds)
					  nil t nil nil)))
      (let ((t-or-f (assoc-default cmds gdb-adb-cmds)))
	(if (functionp t-or-f)
	    (call-interactively t-or-f))))


(provide 'gdb-adb)
