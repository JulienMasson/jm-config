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
(require 'pm-ifwi-intel)
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

;; gdb with
(require 'gdb-adb)

;; ifwi tools
(require 'ifwi-tools)

;; mpta
(require 'mpta-text)

;; magit push gerrit
(defun magit-git-push-gerrit (branch target args)
  (run-hooks 'magit-credential-hook)
  (-let [(remote . target)
         (magit-split-branch-name target)]
    (magit-run-git-async "push" "-v" args remote
                         (format "%s:refs/for/%s" branch target))))

(defun magit-push-gerrit (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (magit-read-local-branch-or-commit "Push")))
     (list source
           (magit-read-remote-branch (format "Push %s to" source) nil
                                     (magit-get-upstream-branch source)
                                     source 'confirm)
           (magit-push-arguments))))
  (magit-git-push-gerrit source target args))

(magit-define-popup-action 'magit-push-popup
  ?g "Gerrit" 'magit-push-gerrit)

(defun adb-dired ()
  (interactive)
  (shell-command-to-string "adb wait-for-device root")
  (setq adb-device-name
	(shell-command-to-string "adb wait-for-device devices | grep -w \"device\" | awk '{ print $1}' | tr -d \"\n\""))
  (let ((default-directory (concat "/adb:" adb-device-name ":/")))
    (ido-find-file)))

;; work keybindings
(require 'work-keybindings)

;; expand tramp remote path
(add-to-list 'tramp-remote-path "/home/jmassonx/bin")

;; startup work stuff
(defun start-work ()
  (interactive)

  ;; status widgets
  (status-add-to-left 'status-purple-conversation)
  (status-add-to-left 'status-gnus-intel-inbox)
  (status-add-to-left 'status-gnus-intel-gerrit)
  (status-add-to-left 'status-gnus-intel-jira)
  (status-add-to-left 'status-gnus-intel-confluence)
  (status-add-to-left 'status-cscope)
  (status-add-to-left 'status-purple-user)
  (status-add-to-left 'status-project-manager)
  (status-add-to-left 'status-virtual-desktops)
  (status-add-to-right 'status-date)
  (status-add-to-right 'status-volume)
  (status-add-to-right 'status-battery)
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
  (purple-init))


(provide 'my-work)
