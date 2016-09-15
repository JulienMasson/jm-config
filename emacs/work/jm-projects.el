(defvar my-usual-subprojects '(("droidboot"	. "/bootable/droidboot")
			       ("droidboot"	. "/vendor/intel/droidboot")
			       ("recovery"	. "/bootable/recovery")
			       ("libintelprov"	. "/vendor/intel/hardware/libintelprov")
			       ("tasks"		. "/vendor/intel/build/tasks")
			       ("kernel"	. "/linux/kernel")
			       ("fugu"		. "/vendor/intel/PRIVATE/fugu")
			       ("mvn"		. "/vendor/intel/PRIVATE/mvn")
			       ("manufacturing"	. "/vendor/intel/PRIVATE/manufacturing")
			       ("init"		. "/system/core/init")
			       ("glacier"	. "/device/intel/glacier")
			       ("grant" 	. "/device/intel/grant")
			       ("robby"	        . "/device/intel/robby")
			       ("anthracite"    . "/device/intel/anthracite")
			       ("shasta"	. "/device/intel/shasta")
			       ("ohrm"  	. "/vendor/intel/hardware/PRIVATE/ohrm")
			       ("sand"		. "/vendor/intel/PRIVATE/sand")))

(setq http-proxy "http://proxy.ir.intel.com:911"
      no-proxy   "localhost,intel.com,10.0.0.0/8,192.168.0.0/16"
      jdk-path   "/usr/lib/jvm/java-7-openjdk-amd64")

(setq aosp-env-vars '(("http_proxy"	.	http-proxy)
		      ("https_proxy"	.	http-proxy)
		      ("ftp_proxy"	.	http-proxy)
		      ("no_proxy"	.	no-proxy)
		      ("PATH"		.	(concat "$PATH:" jdk-path "/bin:/home/lab/bin:/usr/sbin:/sbin:/opt/bin"))
		      ("JAVA_HOME"	.	jdk-path)
		      ("CLASSPATH"	.	".")
		      ("EDITOR"		.	"emacsclient --socket-name /tmp/emacs1000/server")))

(mapc (lambda (x) (setenv (car x) (substitute-env-vars (eval (cdr x))))) aosp-env-vars)


(register-project
 (make-project :name "Robby"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"robby")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "Grant"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"grant")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "Glacier"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"glacier")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "Shasta"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"shasta")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "Anthracite"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"anthracite")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "Mars"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"mars_aosp")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "jm-config"
	       :pm-backend "emacslisp"
	       :root-path "/home/lab/jm-config"
	       :env-vars '()
	       :subprojects '(("work"          .       "/emacs/work")
			      ("home"          .       "/emacs/home")
			      ("modules"       .       "/emacs/modules")
			      ("bash"	       .	"/bash")
			      ("awesome"       .	"/awesome")
			      ("tools"	       .	"/tools"))))

(register-project
 (make-project :name "IFWI"
	       :pm-backend "ifwi-intel"
	       :root-path "/home/lab/Documents/Intel/ndg_ifwi-marvin/"
	       :env-vars '()
	       :subprojects '(("patchs"          .       "/marvin/ia/patches"))))

(register-project
 (make-project :name "IAFW"
	       :pm-backend "ifwi-intel"
	       :root-path "/home/lab/Documents/Intel/mcg_umfdk-umfdk/"
	       :env-vars '()
	       :subprojects '(("splash"          .       "/cs_tangier_src/nc"))))

(register-project
 (make-project :name "SCU"
	       :pm-backend "ifwi-intel"
	       :root-path "/home/lab/Documents/Intel/SCU/"
	       :env-vars '()
	       :subprojects '(("Bootstrap"          .       "/Bootstrap")
			      ("Runtime"            .       "/Runtime"))))

(register-project
 (make-project :name "local"
	       :pm-backend "intel-android"
	       :root-path "/home/lab/Documents/Intel/ndg-android"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"shasta")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	4))
	       :subprojects my-usual-subprojects))


(provide 'jm-projects)
