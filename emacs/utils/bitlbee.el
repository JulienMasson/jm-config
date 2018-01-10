;;; bitlbee.el

;; Copyright (C) 2018 Julien Masson

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)

(defstruct bitlbee
  process
  server
  b-backend)

(defstruct b-backend
  name
  nick
  cmd-args
  port
  (conf-file 'ignore)
  (after-connect 'ignore))

(defvar bitlbee-executable "bitlbee")
(defvar bitlbee-user-directory "~/.bitlbee")
(defvar bitlbee-accounts '())
(defvar bitlbee-buddies nil)
(defvar bitlbee-current nil)

(defun bitlbee-register (backend)
  (let* ((name (b-backend-name backend))
	 (account (make-bitlbee :process (format "*%s*" name)
				:server (format "%s-erc" name)
				:b-backend backend)))
    (add-to-list 'bitlbee-accounts account nil)))

(defun bitlbee-find-account (name)
  (seq-find (lambda (account)
	      (string= (b-backend-name
			(bitlbee-b-backend account))
		       name))
	    bitlbee-accounts))

(defun bitlbee-find-buddy (name)
  (seq-find (lambda (buddy)
	      (string= (cdr buddy)
		       name))
	    bitlbee-buddies))

(defun bitlbee-build-cmd (backend)
  (format " %s -p %s -d %s -c %s "
	  (b-backend-cmd-args backend)
	  (b-backend-port backend)
	  bitlbee-user-directory
	  (b-backend-conf-file backend)))

(defun bitlbee-hook ()
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (let ((backend (bitlbee-b-backend bitlbee-current)))
      (rename-buffer (bitlbee-server bitlbee-current))
      (remove-hook 'erc-join-hook 'bitlbee-hook)
      (call-if-function (b-backend-after-connect backend)))))

(defun bitlbee-list-erc-modified ()
  (delq nil
	(mapcar (lambda (alist)
		  (let ((name (buffer-name (car alist))))
		    (with-current-buffer name
		      (when (and (erc-query-buffer-p)
				 (erc-server-process-alive))
			name))))
		erc-modified-channels-alist)))

(defun bitlbee-list-erc-buffer ()
  (delq nil
	(mapcar (lambda (buffer)
		  (with-current-buffer (buffer-name buffer)
		    (when (and (erc-query-buffer-p)
			       (erc-server-process-alive))
		      (buffer-name buffer))))
		(erc-buffer-list))))

(defun bitlbee-list-open-chat ()
  (delete-dups (append (bitlbee-list-erc-modified)
		       (bitlbee-list-erc-buffer))))

(defun bitlbee-jump (name)
  (interactive (list (ido-completing-read "Jump to: "
					  (bitlbee-list-open-chat)
					  nil t nil nil)))
  (switch-to-buffer name))

(defun bitlbee-running-p (process)
  "Returns non-nil if process is running"
  (if (get-buffer-process process) t nil))

(defun bitlbee-get-buddy (proc parsed)
  (let ((msg (erc-response.contents parsed))
	(match (b-backend-name
		(bitlbee-b-backend bitlbee-current))))
    (when (stringp msg)
      (if (string-match "buddies" msg)
	  (progn
	    (remove-hook 'erc-server-PRIVMSG-functions 'bitlbee-get-buddy)
	    (setq bitlbee-buddies (mapcar (lambda (buddy)
					    `(,bitlbee-current . ,buddy))
					  bitlbee-buddies))
	    (message (propertize "Update list done" 'face 'success)))
	(save-match-data
	  (if (string-match (format "\\(\\w*\\)\\s-*[0-9]* %s\\s-*\\(\\w*\\)$" match) msg)
	      (let ((buddy-face (if (string= "Online" (match-string 2 msg))
				    'success
				  'error)))
	      (delete-dups (push (propertize (match-string 1 msg) 'face buddy-face)
				 bitlbee-buddies)))))))))

(defun bitlbee-update-list (name)
  (interactive (list (ido-completing-read "Update: "
					  (delq nil (mapcar (lambda (account)
							      ;; (if (bitlbee-running-p (bitlbee-process account))
								  (b-backend-name (bitlbee-b-backend account)))
							    bitlbee-accounts))
					  nil t nil nil)))
  (let ((account (bitlbee-find-account name)))
    (setq bitlbee-current account)
    (setq bitlbee-buddies nil)
    (add-hook 'erc-server-PRIVMSG-functions 'bitlbee-get-buddy)
    (with-current-buffer (bitlbee-server account)
      (erc-send-message "blist all"))))

;; bitlbee actions
(defvar bitlbee-actions '(("start"       . bitlbee-start)
			  ("chat"        . bitlbee-chat)
			  ("jump"        . bitlbee-jump)
			  ("update-list" . bitlbee-update-list)
			  ("quit"        . bitlbee-quit)))

(defun bitlbee-start (name)
  (interactive (list (ido-completing-read "Start: "
					  (mapcar (lambda (account)
						    (b-backend-name (bitlbee-b-backend account)))
						  bitlbee-accounts)
					  nil t nil nil)))
  (let* ((account (bitlbee-find-account name))
	 (backend (bitlbee-b-backend account))
	 (cmd-line (bitlbee-build-cmd backend)))
    (start-process-shell-command name
    				 (bitlbee-process account)
    				 bitlbee-executable
    				 cmd-line)
    (erc :server "localhost"
	 :port (b-backend-port backend)
	 :nick (b-backend-nick backend))
    (setq bitlbee-current account)
    (add-hook 'erc-join-hook 'bitlbee-hook)))

(defun bitlbee-connect (server name)
  (with-current-buffer server
    (let ((session-buffer (erc-server-buffer))
          (erc-join-buffer erc-query-display))
      (if name
          (erc-query name session-buffer)
	(signal 'wrong-number-of-arguments "")))))

(defun bitlbee-chat (name)
  (interactive (list (ido-completing-read "Chat to: "
					  (mapcar 'cdr bitlbee-buddies)
					  nil t nil nil)))

  (let* ((buddy (bitlbee-find-buddy name))
	 (server (bitlbee-server (car buddy))))
    (bitlbee-connect server name)))

(defun bitlbee-quit (name)
  (interactive (list (ido-completing-read "Quit: "
					  (mapcar (lambda (account)
						    (b-backend-name (bitlbee-b-backend account)))
						  bitlbee-accounts)
					  nil t nil nil)))
  (let* ((account (bitlbee-find-account name))
	 (buffer-process (bitlbee-process account))
	 (kill-buffer-query-functions (remq 'process-kill-buffer-query-function
					    kill-buffer-query-functions)))
    (when (get-buffer buffer-process)
      (kill-buffer buffer-process))
    (mapcar #'kill-buffer (erc-buffer-list))))

(defun bitlbee (action)
  (interactive (list (ido-completing-read "Bitlbee: "
					  (mapcar 'car bitlbee-actions)
					  nil t nil nil)))
  (let ((t-or-f (assoc-default action bitlbee-actions)))
    (if (functionp t-or-f)
        (call-interactively t-or-f))))


(provide 'bitlbee)
