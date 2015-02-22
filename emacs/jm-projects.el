(defvar my-usual-subprojects '(("iafw"		. "/vendor/intel/fw/PRIVATE/byt-iafw")
			       ("libintelprov"	. "/vendor/intel/hardware/libintelprov")
			       ("tasks"		. "/vendor/intel/build/tasks")
			       ("kernelflinger" . "/hardware/intel/kernelflinger")
                               ("gnu-efi"       . "/external/gnu-efi/gnu-efi-3.0")
			       ("userfastboot"	. "/bootable/userfastboot")
			       ("mixins"	. "/device/intel/mixins/groups")
			       ("uefi"		. "/bootable/uefi/")
			       ("droidboot"	. "/bootable/droidboot")
			       ("recovery"	. "/bootable/recovery")
			       ("init"		. "/system/core/init")
			       ("fastboot"	. "/system/core/fastboot")
			       ("linux"		. "/linux/kernel")))

(setq http-proxy "http://proxy.ir.intel.com:911"
      no-proxy   "localhost,intel.com,10.0.0.0/8,192.168.0.0/16"
      jdk-path   "/usr/lib/jvm/java-7-openjdk-amd64")

(setq aosp-env-vars '(("http_proxy"	.	http-proxy)
		      ("https_proxy"	.	http-proxy)
		      ("ftp_proxy"	.	http-proxy)
		      ("no_proxy"	.	no-proxy)
		      ("PATH"		.	(concat "$PATH:" jdk-path "/bin:$HOME/bin:/usr/sbin:/sbin:/opt/bin"))
		      ("JAVA_HOME"	.	jdk-path)
		      ("CLASSPATH"	.	".")
		      ("EDITOR"		.	"emacsclient --socket-name /tmp/emacs1000/server")))

(mapc (lambda (x) (setenv (car x) (substitute-env-vars (eval (cdr x))))) aosp-env-vars)


(register-project
 (make-project :name "Imin_legacy - Sand"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabX2:/build/jmassonx/imin-legacy-scr"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_sand")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))


(register-project
 (make-project :name "Imin_legacy - Fugu"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabX2:/build/jmassonx/imin-legacy"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_fugu")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))


(register-project
 (make-project :name "intel-lisp"
	       :pm-backend "emacslisp"
	       :root-path "/home/lab/Document/Intel/intel-lisp"
	       :env-vars '()
	       :subprojects '(("activity"		.	"/activity")
			      ("device-control"		.	"/device-control")
			      ("gerrit"			.	"/gerrit")
			      ("log-tools"		.	"/log-tools")
			      ("project-manager"	.	"/project-manager")
			      ("purple"			.	"/purple")
			      ("search-tools"		.	"/search-tools"))))


(provide 'jm-projects)
