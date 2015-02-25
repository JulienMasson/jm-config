;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                WORK CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mail
(require 'work-wl)

;; latex config
;; (require 'my-latex)

;; Log management
(require 'log-tools)
(require 'lt-serial-kernel)
(require 'lt-logcat)
(require 'lt-serial)

;; Project manager
(require 'project-manager)
(require 'pm-emacslisp)
(require 'pm-android-intel)
(require 'jm-projects)

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

;; search tools
(require 'search-engine)
(require 'search-tools)

;; Ldap tool
(require 'ldap-browser)
(require 'ldap-browser-mail)
(require 'ldap-browser-purple)
(defvar ldap-servers '(("ger.corp.intel.com"	.	"ou=Workers,dc=ger,dc=corp,dc=intel,dc=com")
		       ("amr.corp.intel.com"	.	"ou=Workers,dc=amr,dc=corp,dc=intel,dc=com")
		       ("gar.corp.intel.com"	.	"ou=Workers,dc=gar,dc=corp,dc=intel,dc=com")
		       ("ccr.corp.intel.com"	.	"ou=Workers,dc=ccr,dc=corp,dc=intel,dc=com")))

;; Load desktop settings
(require (intern (system-name)) nil t)

;; org jira tools
(require 'org-jira)
(setq jiralib-host "jira01.devtools.intel.com"
      jiralib-url "https://jira01.devtools.intel.com")

;; Automatic emacsclient
;; (add-to-list 'load-path "~/jm-config/emacs/modules/tramp/lisp")
;; (require 'tramp)
;; (require 'tramp-sh)
;; (defvar emacsserver-file-path "~/emacs-server")
;; (defvar emacsclient-path "~/emacsclient.sh")
;; (defvar emacsclient-script (format "#!/bin/sh\n\
;; \n\
;; emacsclient --server-file %s \\\n\
;;             /ssh:jmassonx@$(hostname):`readlink --canonicalize-missing $*`\n"
;; 				   emacsserver-file-path))

;; (dolist (var '("EDITOR" "GIT_EDITOR" "SVN_EDITOR"))
;;   (add-to-list 'tramp-remote-process-environment (format "%s=%s" var emacsclient-path)))

;; (defvar server-interface-priority-order '("vpn0" "eth0" "wlan0"))
;; (defun reconfigure-server ()
;;   (interactive)
;;   (let ((interface (car (delq nil (mapcar (rcurry 'assoc (network-interface-list))
;; 					  server-interface-priority-order)))))
;;     (when interface
;;       (setq server-use-tcp t
;; 	    server-port 5001
;; 	    server-host (format-network-address (cdr interface) t))
;;       (server-start))))

;; (defun setup-emacsclient (proc vec)
;;   (when (and (server-running-p) server-use-tcp)
;;     (with-temp-buffer
;;       (insert-file (format "%s/server" server-auth-dir))
;;       (dolist (cur '(("\"" . "\\\"") ("$" . "\\$") ("\`" . "\\`")))
;; 	(save-excursion
;; 	  (while (search-forward (car cur) nil t)
;; 	    (replace-match (cdr cur) nil t))))
;;       (shell-command (format "echo -n \"%s\" > %s" (buffer-string) emacsserver-file-path)))
;;     (tramp-send-command vec (format "echo '%s' > %s" emacsclient-script emacsclient-path))
;;     (tramp-send-command vec (format "chmod +x %s" emacsclient-path))))

;; (advice-add 'tramp-open-connection-setup-interactive-shell :after #'setup-emacsclient)
;; (reconfigure-server)

(require 'work-keybindings)


(provide 'my-work)
