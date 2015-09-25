(require 'ido)
(require 'device-control)
(require 'device-control-adb)
(require 'device-control-fastboot)
(require 'device-control-flashrom)
(require 'device-control-relay)

(add-to-list 'dctrl-adb-reboot-target "dnx" t 'string=)

(dolist (image '(("fastboot"	.	"droidboot.img")
		 ("manufacturing".	"manufacturing.img")
		 ("oem"         .	"oem.img")
		 ("ESP"		.	"ESP.img")
		 ("osloader"	.       "efilinux-eng.efi")
		 ("capsule"	.	"capsule.bin")))
  (add-to-list 'dctrl-fastboot-flash-alist image t
	       (lambda (x y) (string= (car x) (car y)))))

(defun dctrl-intel-action-adb-kill-wizard ()
  (dctrl-adb-run "shell" "pm" "disable" "com.google.android.setupwizard"))

(defun dctrl-intel-action-adb-adb2fastboot ()
  (dctrl-adb-run "shell" "setprop" "sys.adb.config" "fastboot"))

(defvar dctrl-intel-fastboot-type-history '())

(defun dctrl-intel-action-fastboot-interactive-flash ()
  (let* ((type (read-string "Type: " nil 'dctrl-intel-fastboot-type-history))
	 (file (ido-read-file-name "File: "))
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "File to flash: " (dctrl-fastboot-aosp-out-dir)
				     (assoc-default type dctrl-fastboot-flash-alist) t)))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-fastboot-run "flash" type (expand-file-name ctrlhost-filename)))))

(defun dctrl-intel-action-fastboot-fastboot2adb ()
  (dctrl-fastboot-run "oem" "fastboot2adb"))

(defun dctrl-intel-action-intel-phoneflashtool (&optional file)
  (let* ((path (concat aosp-path "/pub/" aosp-board-name "/flash_files/"))
	 (file (expand-file-name (or file (ido-read-file-name "FlashFile: " path))))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-run-process (list "phoneflashtool-cli-launcher.sh" "--always-unzip" "-f" ctrlhost-filename)))))

(defun dctrl-intel-action-intel-adb-to-efi-shell ()
  (dctrl-adb-run "shell" "uefivar" "-g" "8be4df61-93ca-11d2-aa0d-00e098032b8c" "-n" "BootNext" "-t" "int16" "-s" "1"))

(defun dctrl-intel-get-actions ()
  (dctrl-agregate-fun-list (dctrl-build-fun-list "dctrl-intel-action-adb-"
						 (if (dctrl-adb-connected-p) 'success 'error))
			   (dctrl-build-fun-list "dctrl-intel-action-fastboot-"
						 (if (dctrl-fastboot-connected-p) 'success 'error))
			   (dctrl-build-fun-list "dctrl-intel-action-intel-" 'success)
			   (dctrl-adb-get-actions)
			   (dctrl-fastboot-get-actions)
			   (dctrl-flashrom-get-actions)
			   (dctrl-relay-get-actions)))

(defun dctrl-intel-guess-device-names ()
  (nconc (dctrl-adb-guess-device-names)
	 (dctrl-fastboot-guess-device-names)))

(defun dctrl-intel-init ()
  (dctrl-relay-init))

(dctrl-register-backend
 (make-dctrl-backend :name "intel"
		     :create 'dctrl-intel-init
		     :get-actions 'dctrl-intel-get-actions
		     :guess-device-names 'dctrl-intel-guess-device-names))

(provide 'device-control-intel)
