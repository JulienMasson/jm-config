(defvar my-usual-subprojects '(("droidboot"	. "/bootable/droidboot")
			       ("libintelprov"	. "/vendor/intel/hardware/libintelprov")
			       ("tasks"		. "/vendor/intel/build/tasks")
			       ("fugu"		. "/vendor/intel/PRIVATE/fugu")
			       ("sand"		. "/vendor/intel/PRIVATE/sand")
			       ("linux"		. "/linux/kernel")))

(setq http-proxy "http://proxy.ir.intel.com:911"
      no-proxy   "localhost,intel.com,10.0.0.0/8,192.168.0.0/16"
      jdk-path   "/usr/lib/jvm/java-7-openjdk-amd64")

(setq aosp-env-vars '(("http_proxy"	.	http-proxy)
		      ("https_proxy"	.	http-proxy)
		      ("ftp_proxy"	.	http-proxy)
		      ("no_proxy"	.	no-proxy)
		      ("PATH"		.	(concat "$PATH:" jdk-path "/bin:/home/jmassonx/bin:/usr/sbin:/sbin:/opt/bin"))
		      ("JAVA_HOME"	.	jdk-path)
		      ("CLASSPATH"	.	".")
		      ("EDITOR"		.	"emacsclient --socket-name /tmp/emacs1000/server")))

(mapc (lambda (x) (setenv (car x) (substitute-env-vars (eval (cdr x))))) aosp-env-vars)


(register-project
 (make-project :name "FULL - Sand"
	       :pm-backend "intel-android"
	       :root-path "/ssh:jmassonx@tllabX2:/build/jmassonx/imin-legacy"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_sand")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))


(register-project
 (make-project :name "FULL - Fugu"
	       :pm-backend "intel-android"
	       :root-path "/ssh:jmassonx@tllabX2:/build/jmassonx/imin-legacy"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_fugu")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "IMIN - Fugu"
	       :pm-backend "intel-android"
	       :root-path "/ssh:jmassonx@tllabX2:/build/jmassonx/imin-legacy"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"imin_fugu")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "MR1 - Fugu"
	       :pm-backend "intel-android"
	       :root-path "/ssh:jmassonx@tllabX2:/build/jmassonx/mr1"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_fugu")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "jm-config"
	       :pm-backend "emacslisp"
	       :root-path "/home/lab/jm-config"
	       :env-vars '()
	       :subprojects '(("emacs"          .       "/emacs")
			      ("bash"	        .	"/bash")
			      ("awesome"	.	"/awesome")
			      ("others"		.	"/others"))))

(provide 'jm-projects)
