;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                WORK CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mail
(require 'work-gnus)

;; Chat interface for pidgin
(require 'purple)

;; Status
(require 'status)
(setq status-separator  " | ")
(setq status-battery-discharging-fmt " %p%% %t")
(setq status-gnus-groups '("nnmaildir+Intel:INBOX"
                           "nnmaildir+Intel:Gerrit"
                           "nnmaildir+Intel:Jira"
                           "nnmaildir+Intel:Confluence"))
(setq status-gnus-separator " - ")
(setq status-gnus-medium-threshold 5)
(setq status-gnus-high-threshold 10)
(setq net-interfaces '("wlan0"))

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

;; intel tools
(require 'intel-lib)
(require 'dmesg)
(require 'ifwi-tools)
(require 'mpta)
(require 'locate)

(defun adb-dired ()
  (interactive)
  (shell-command-to-string "adb wait-for-device root")
  (setq adb-device-name
	(shell-command-to-string "adb wait-for-device devices | grep -w \"device\" | awk '{ print $1}' | tr -d \"\n\""))
  (let ((default-directory (concat "/adb:" adb-device-name ":/")))
    (ido-find-file)))
;; add partition for device control
(add-to-list 'dctrl-fastboot-partition-list "manufacturing")
(add-to-list 'dctrl-fastboot-partition-list "mfg_sys")
(add-to-list 'dctrl-fastboot-partition-list "oem")

;; work keybindings
(require 'work-keybindings)

;; connect to marvin irc
(add-to-list 'erc-autojoin-channels-alist '("marvin.net" "#marvin"))
(defun irc-marvin ()
  (interactive)
  (erc :server "tldlab401.tl.intel.com" :port 6667 :nick "jmasson"))

;; startup work stuff
(defun clean-work ()
  (interactive)
  (mapcar #'(lambda (buffer)
	      (let ((file (buffer-name buffer)))
		(when (and file (string-match ".*\\.[h|c]" file))
		  (kill-buffer file))))
	  (buffer-list)))

(defun start-work ()
  (interactive)

  ;; status widgets
  (status-add-to-left 'status-erc)
  (status-add-to-left 'status-purple-conversation)
  (status-add-to-left 'status-gnus)
  (status-add-to-left 'status-cscope)
  (status-add-to-left 'status-purple-user)
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
  (purple-init))


(provide 'my-work)
