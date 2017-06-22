(require 'ido)
(require 'device-control)
(require 'device-control-adb)
(require 'device-control-fastboot)
(require 'device-control-flashrom)
(require 'device-control-relay)

(dolist (image '(("fastboot"	.	"droidboot.img")
		 ("manufacturing".	"manufacturing.img")
		 ("bootloader"  .	"bootloader.img")
		 ("cache"       .	"cache.img")
		 ("oem"         .	"oem.img")
		 ("mfg_sys"     .	"mfg_sys.img")))
  (add-to-list 'dctrl-fastboot-flash-alist image t
	       (lambda (x y) (string= (car x) (car y)))))

(setq dctrl-fastboot-flash-alist '(("boot" . "boot.img")
 ("bootloader" . "bootloader.img")
 ("recovery" . "recovery.img")
 ("system" . "system.img")
 ("zimage" . "kernel")
 ("fastboot" . "droidboot.img")
 ("manufacturing" . "manufacturing.img")
 ("cache" . "cache.img")
 ("mfg_sys" . "mfg_sys.img")
 ("oem" . "oem.img")))


(defvar dctrl-intel-custom-boot-targets
  '(("MOS" . "1")
    ("TOS" . "2")
    ("POS" . "3")))

(defun dctrl-fastboot-action-custom-boot (&optional target)
  (let* ((target (or target (ido-completing-read "Target: " (mapcar 'car dctrl-intel-custom-boot-targets) nil t))))
    (dctrl-fastboot-run "oem" "custom_boot" (assoc-default target dctrl-intel-custom-boot-targets))))

(defun dctrl-intel-action-adb-kill-wizard ()
  (dctrl-adb-run "shell" "pm" "disable" "com.google.android.setupwizard"))

(defun dctrl-intel-action-adb-adb2fastboot ()
  (dctrl-adb-run "shell" "setprop" "sys.adb.config" "fastboot"))

(defun dctrl-intel-action-adb-install (&optional file)
  (let* ((file file)
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "APK install: " (concat (dctrl-fastboot-aosp-out-dir) "system/priv-app/httpget-release/") "httpget-release.apk" t)))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-adb-run "install -r " (expand-file-name ctrlhost-filename)))))

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

(defun dctrl-intel-action-fastboot-ifwi (&optional file)
    (let* ((file file)
	 tramp-cmd ctrlhost-filename)
    (unless (and file (file-exists-p file))
      (setq file (ido-read-file-name "Flash ifwi: " (concat aosp-path "/vendor/intel/PRIVATE/mvn/fw/" aosp-board-name "/") "marvin_ifwi.bin" t)))
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-fastboot-run "flash" "ifwi" (expand-file-name ctrlhost-filename)))))

(defun dctrl-intel-action-intel-phoneflashtool (&optional file)
  (let* ((path (concat aosp-path "/pub/" aosp-board-name "/flash_files/"))
	 (file (expand-file-name (or file (ido-read-file-name "FlashFile: " path))))
	 tramp-cmd ctrlhost-filename)
    (multiple-value-setq (tramp-cmd ctrlhost-filename)
      (dctrl-untramp-file file))
    (append tramp-cmd
	    (dctrl-run-process (list "platformflashtool-cli-launcher.sh" "--always-unzip" "-f" ctrlhost-filename)))))

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
