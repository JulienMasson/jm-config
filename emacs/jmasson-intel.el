;; Adapt and add the following lines to your .emacs file

;; (add-to-list 'load-path "~/Intel/intel-lisp")
;; (let ((default-directory "~/Intel/intel-lisp"))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (require 'my-intel)

;; Log management
(require 'log-tools)
(require 'lt-serial-kernel)
(require 'lt-logcat)
(require 'lt-serial)

;; Project manager
(require 'project-manager)
(require 'pm-emacslisp)
(require 'pm-android-intel)

;; Gerrit
(require 'gerrit)
(require 'repo)
(require 'org-gerrit)

;; Device control
(require 'device-control)
(require 'device-control-flashrom)
(require 'device-control-adb)
(require 'device-control-fastboot)
(require 'device-control-relay)
(require 'device-control-android)
(require 'device-control-intel)

;; Ldap tool
(require 'ldap-browser)
(require 'ldap-browser-mail)
(require 'ldap-browser-purple)
(defvar ldap-servers '(("ger.corp.intel.com"	.	"ou=Workers,dc=ger,dc=corp,dc=intel,dc=com")
		       ("amr.corp.intel.com"	.	"ou=Workers,dc=amr,dc=corp,dc=intel,dc=com")
		       ("gar.corp.intel.com"	.	"ou=Workers,dc=gar,dc=corp,dc=intel,dc=com")
		       ("ccr.corp.intel.com"	.	"ou=Workers,dc=ccr,dc=corp,dc=intel,dc=com")))

;; Activity window manager
;;(require 'activity)

;; Load desktop settings
(require (intern (system-name)) nil t)

;; org jira tools
(require 'org-jira)
(setq jiralib-host "jira01.devtools.intel.com"
      jiralib-url "https://jira01.devtools.intel.com")

;; intel functions
(defun intel-adb-devices ()
  (interactive)
  (shell-command (format "adb devices")))

(defun intel-adb-root ()
  (interactive)
  (shell-command (format "adb root")))

(defun intel-adb-reboot ()
  (interactive)
  (shell-command (format "adb shell reboot")))

(defun intel-adb-shutdown ()
  (interactive)
  (shell-command (format "adb shell reboot -p shutdown")))

(defun intel-adb-reboot-bootloader ()
  (interactive)
  (shell-command (format "adb reboot bootloader")))

(defun intel-adb-reboot-recovery ()
  (interactive)
  (shell-command (format "adb reboot recovery")))

(defun intel-fastboot-devices ()
  (interactive)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S fastboot devices")))

(defun intel-fastboot-reboot-bootloader ()
  (interactive)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S fastboot reboot-bootloader")))

(defun intel-fastboot-flash-boot (file-path)
  (interactive "FBoot image: ")
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S fastboot flash boot " file-path)))

(defun intel-fastboot-flash-system (file-path)
  (interactive "FSystem image: ")
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S fastboot flash system " file-path)))

(defun intel-fastboot-flash-droidboot (file-path)
  (interactive "FDroidboot image: ")
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S fastboot flash fastboot " file-path)))

(defun intel-fastoot-continue ()
  (interactive)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
			 " | sudo -S fastboot continue")))

(defun intel-grant-loglevel ()
  (interactive)
  (intel-adb-root)
  (setq cmdline-string (shell-command-to-string "adb shell /sbin/kcmdline --get /dev/block/by-name/boot"))
  (setq cmdline-string-raised (replace-regexp-in-string "loglevel=." "loglevel=8" cmdline-string))
  (shell-command "adb shell /sbin/kcmdline --set 'init=/init pci=noearly console=logk0 console=ttyS0 earlyprintk=nologger panic_on_bad_page=1 panic_on_list_corruption=1 loglevel=8 vmalloc=256M androidboot.hardware=mofd_v1 androidboot.spid=xxxx:xxxx:xxxx:xxxx:xxxx:xxxx androidboot.serialno=01234567890123456789 androidboot.selinux=permissive snd_pcm.maximum_substreams=8 ptrace.ptrace_can_access=1 allow_factory=1 ip=50.0.0.2:50.0.0.1::255.255.255.0::usb0:on debug_locks=0 n_gsm.mux_base_conf=\"ttyACM0,0 ttyXMM0,1\" bootboost=1' /dev/block/by-name/boot")
  ;; (shell-command
  ;;  (concat "adb shell /sbin/kcmdline --set '"
  ;; 	   (shell-quote-argument cmdline-string-raised)
  ;; 	   "' /dev/block/by-name/boot"))
  (message cmdline-string-raised))

;; intel shortcuts
(global-set-key (kbd "C-c a d a") 'intel-adb-devices)
(global-set-key (kbd "C-c a g") 'intel-adb-root)
(global-set-key (kbd "C-c a r") 'intel-adb-reboot)
(global-set-key (kbd "C-c a s") 'intel-adb-shutdown)
(global-set-key (kbd "C-c a b") 'intel-adb-reboot-bootloader)
;;(global-set-key (kbd "C-c a r r") 'intel-adb-reboot-recovery)
(global-set-key (kbd "C-c a d f") 'intel-fastboot-devices)
(global-set-key (kbd "C-c a f r") 'intel-fastboot-reboot-bootloader)
(global-set-key (kbd "C-c a f b") 'intel-fastboot-flash-boot)
(global-set-key (kbd "C-c a f s") 'intel-fastboot-flash-system)
(global-set-key (kbd "C-c a f d") 'intel-fastboot-flash-droidboot)
(global-set-key (kbd "C-c a c") 'intel-fastoot-continue)
(global-set-key (kbd "C-c a l") 'intel-grant-loglevel)


(provide 'jmasson-intel)
