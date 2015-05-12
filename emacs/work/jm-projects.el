(defvar my-usual-subprojects '(("droidboot"	. "/bootable/droidboot")
			       ("droidboot"	. "/vendor/intel/droidboot")
			       ("libintelprov"	. "/vendor/intel/hardware/libintelprov")
			       ("tasks"		. "/vendor/intel/build/tasks")
			       ("kernel"	. "/linux/kernel")
			       ("fugu"		. "/vendor/intel/PRIVATE/fugu")
			       ("mvn"		. "/vendor/intel/PRIVATE/mvn")
			       ("marvin"	. "/device/intel/marvin")
			       ("sand"		. "/vendor/intel/PRIVATE/sand")))

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
 (make-project :name "Sand"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/r51-stable"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_sand")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))


(register-project
 (make-project :name "Fugu"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/r51-stable"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"full_fugu")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "Marvin"
	       :pm-backend "intel-android"
	       :root-path "/ssh:tllabx2:/build/jmassonx/r44b_mvn"
	       :env-vars '((aosp-path		.	(project-root-path current-project))
			   (aosp-board-name	.	"marvin")
			   (aosp-build-variant	.	"userdebug")
			   (aosp-thread-number	.	32))
	       :subprojects my-usual-subprojects))

(register-project
 (make-project :name "jm-config"
	       :pm-backend "emacslisp"
	       :root-path "/home/jmassonx/jm-config"
	       :env-vars '()
	       :subprojects '(("work"          .       "/emacs/work")
			      ("home"          .       "/emacs/home")
			      ("modules"       .       "/emacs/modules")
			      ("bash"	       .	"/bash")
			      ("awesome"       .	"/awesome")
			      ("tools"	       .	"/tools"))))

(provide 'jm-projects)
