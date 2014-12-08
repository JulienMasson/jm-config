;; Adapt and add the following lines to your .emacs file

;; (add-to-list 'load-path "~/Intel/intel-lisp")
;; (let ((default-directory "~/Intel/intel-lisp"))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (require 'my-intel)

;; Status
(require 'status)

;; Log management
(require 'log-tools)
(require 'lt-serial-kernel)
(require 'lt-logcat)
(require 'lt-serial)

;; Project manager
(require 'project-manager)
(require 'pm-emacslisp)
(require 'pm-android-intel)

;; Offlineimap
(require 'offlineimap)

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

(provide 'jmasson-intel)
