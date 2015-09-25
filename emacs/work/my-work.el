;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                WORK CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mail
(require 'work-gnus)

;; Chat interface for pidgin
(require 'purple)

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

;; org jira tools
(require 'org-jira)
(setq jiralib-host "jira.ndg.intel.com"
      jiralib-url "https://jira.ndg.intel.com")

;; magit push gerrit
(defun magit-push-gerrit (branch remote &optional remote-branch args)
  "Push a branch to gerrit"
  (interactive (magit-push-read-args t))
  (magit-run-git-async-no-revert
   "push" "-v" args remote
   (if remote-branch
       (format "%s:refs/for/%s" branch remote-branch)
     branch)))

(magit-define-popup-action 'magit-push-popup
  ?g "Gerrit" 'magit-push-gerrit)

;; work keybindings
(require 'work-keybindings)

;; expand tramp remote path
(add-to-list 'tramp-remote-path "/home/jmassonx/bin")

;; startup work stuff
(defun start-work ()
  (interactive)

  ;; status widgets
  (status-add-to-left 'status-purple)
  (status-add-to-left 'status-gnus)
  (status-add-to-left 'status-cscope)
  (status-add-to-left 'status-project-manager)
  (status-add-to-left 'status-virtual-desktops)
  (status-add-to-right 'status-date)
  (status-add-to-right 'status-volume)
  (status-add-to-right 'status-battery)
  (status-add-to-right 'status-cpu)
  (status-add-to-right 'status-mem)
  (status-add-to-right 'status-net)
  (turn-on-status)

  ;; open files for work at startup
  (find-file "~/org/notes.org")
  (find-file "~/org/todo.org")

  ;; switch to scratch buffer
  (switch-to-buffer "*scratch*")

  ;; add 4 virtual desktop
  (dotimes (i 4)
    (virtual-desktops-add 1))
  (virtual-desktops-goto 1)

  ;; purple init
  (purple-init)

  ;; start rdm
  (rtags-start-process-maybe))


(provide 'my-work)
