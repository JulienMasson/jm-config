;;; echat-pidgin.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2020-04-23

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'dbus)
(require 'shr)

;;; Customization

(defcustom echat-pidgin-name-alias nil
  "An alist mapping a name to username from pidgin."
  :type 'alist
  :group 'echat)

;;; Class

(defclass echat-pidgin (echat)
  ((id      :initarg :id      :initform 0   :type number)
   (type    :initarg :type    :initform ""  :type string)
   (me      :initarg :me      :initform ""  :type string)
   (buddies :initarg :buddies :initform nil)
   (chats   :initarg :chats   :initform nil)))

(defclass echat-pidgin-buddy ()
  ((id      :initarg :id      :initform 0   :type number)
   (name    :initarg :name    :initform ""  :type string)
   (alias   :initarg :alias   :initform ""  :type string)))

(defclass echat-pidgin-chat ()
  ((id      :initarg :id      :initform 0   :type number)
   (name    :initarg :name    :initform ""  :type string)))

;;; Pidgin mode

(define-derived-mode pidgin-im-mode lui-mode "Pidgin IM"
  "Pidgin IM major mode."
  (lui-set-prompt lui-prompt-string)
  (add-hook 'kill-buffer-hook #'echat-pidgin--killed nil t))

(define-derived-mode pidgin-chat-mode lui-mode "Pidgin Chat"
  "Pidgin Chat major mode."
  (lui-set-prompt lui-prompt-string)
  (add-hook 'kill-buffer-hook #'echat-pidgin--killed nil t))

;;; Internal Variables

(defconst pidgin-dbus-service "im.pidgin.purple.PurpleService")
(defconst pidgin-dbus-object "/im/pidgin/purple/PurpleObject")
(defconst pidgin-dbus-interface "im.pidgin.purple.PurpleInterface")

(defvar echat-pidgin--signals
  '(("AccountSignedOn"     . echat-pidgin--account-signed-on)
    ("AccountSignedOff"	   . echat-pidgin--account-signed-off)
    ("WroteImMsg"          . echat-pidgin--im-wrote)
    ("WroteChatMsg"        . echat-pidgin--chat-wrote)))

(defvar echat-pidgin--dbus-objects nil)

(defvar-local echat-pidgin--id nil)

;;; Internal Functions

(defun pidgin--call-method-async (method handler &rest args)
  (apply 'dbus-call-method-asynchronously :session pidgin-dbus-service
	 pidgin-dbus-object pidgin-dbus-interface method handler args))

(defun pidgin--call-method (method &rest args)
  (apply 'dbus-call-method :session pidgin-dbus-service
	 pidgin-dbus-object pidgin-dbus-interface method args))

(defun echat-pidgin--find-by-id (id)
  (cl-find-if (lambda (echat)
		(and (echat-pidgin-p echat)
		     (= (oref echat id) id)))
	      echats))

(defun echat-pidgin--find-buffer (pidgin name)
  (let ((buffer (echat-pidgin--buffer-name pidgin name)))
    (catch 'found
      (dolist (echat-buffer (oref pidgin buffers))
	(when (eq buffer (oref echat-buffer buffer))
	  (throw 'found buffer))))))

(defun echat-pidgin--find-buffer-by-id (pidgin id)
  (catch 'found
    (dolist (echat-buffer (oref pidgin buffers))
      (let ((buffer (oref echat-buffer buffer)))
	(when (buffer-live-p buffer)
	  (with-current-buffer buffer
	    (when (= id echat-pidgin--id)
	      (throw 'found (current-buffer)))))))))

(defun echat-pidgin--find-buddy (pidgin name)
  (cl-find-if (lambda (b) (string= (oref b name) name)) (oref pidgin buddies)))

;; Send

(defun echat-pidgin--im-send (msg)
  (let ((conv-id (pidgin--call-method "PurpleConvIm" :int32 echat-pidgin--id)))
    (pidgin--call-method "PurpleConvImSend" :int32 conv-id :string msg)))

(defun echat-pidgin--chat-send (msg)
  (let ((conv-id (pidgin--call-method "PurpleConvChat" :int32 echat-pidgin--id)))
    (pidgin--call-method "PurpleConvChatSend" :int32 conv-id :string msg)))

;; Buffer

(defun echat-pidgin--killed ()
  (pidgin--call-method "PurpleConversationDestroy" :int32 echat-pidgin--id))

(defun echat-pidgin--buffer-name (pidgin conv-name)
  (with-slots (name type) pidgin
    (format "*%s - %s: %s*" type name conv-name)))

(defun echat-pidgin--im-buffer (pidgin sender id)
  (if-let ((buffer (echat-pidgin--find-buffer-by-id pidgin id)))
      buffer
    (let ((buffer-name (echat-pidgin--buffer-name pidgin sender)))
      (when-let ((buffer (get-buffer buffer-name)))
	(kill-buffer buffer))
      (with-current-buffer (get-buffer-create buffer-name)
	(pidgin-im-mode)
	(setq echat-pidgin--id id)
	(setq lui-input-function #'echat-pidgin--im-send)
	(goto-char (point-max))
	(echat-add-buffer pidgin sender (current-buffer))
	(current-buffer)))))

(defun echat-pidgin--chat-buffer-name (pidgin buffer name)
  (with-current-buffer buffer
    (rename-buffer (echat-pidgin--buffer-name pidgin name))
    (echat-add-buffer pidgin name (current-buffer))))

(defun echat-pidgin--chat-buffer-topic (pidgin buffer id topic)
  (if (string= topic "")
      (pidgin--call-method-async "PurpleConversationGetName"
				 (apply-partially #'echat-pidgin--chat-buffer-name
						  pidgin buffer)
				 :int32 id)
    (with-current-buffer buffer
      (rename-buffer (echat-pidgin--buffer-name pidgin topic))
      (echat-add-buffer pidgin topic (current-buffer)))))

(defun echat-pidgin--chat-get-topic (pidgin buffer id chat-id)
  (pidgin--call-method-async "PurpleConvChatGetTopic"
			     (apply-partially #'echat-pidgin--chat-buffer-topic
					      pidgin buffer id)
			     :int32 chat-id))

(defun echat-pidgin--chat-buffer-infos (pidgin buffer id)
  (pidgin--call-method-async "PurpleConvChat"
			     (apply-partially #'echat-pidgin--chat-get-topic
					      pidgin buffer id)
			     :int32 id))

(defun echat-pidgin--chat-buffer (pidgin id)
  (if-let ((buffer (echat-pidgin--find-buffer-by-id pidgin id)))
      buffer
    (let* ((name (number-to-string id))
	   (buffer-name (echat-pidgin--buffer-name pidgin name)))
      (when-let ((buffer (get-buffer buffer-name)))
	(kill-buffer buffer))
      (with-current-buffer (get-buffer-create buffer-name)
	(echat-pidgin--chat-buffer-infos pidgin (current-buffer) id)
	(pidgin-chat-mode)
	(setq echat-pidgin--id id)
	(setq lui-input-function #'echat-pidgin--chat-send)
	(goto-char (point-max))
	(current-buffer)))))

;; Write

(defun echat-pidgin--buddy-alias (pidgin name)
  (if-let ((buddy (echat-pidgin--find-buddy pidgin name)))
      (oref buddy alias)
    name))

(defun echat-pidgin--html-rendering (msg)
  (with-temp-buffer
    (insert msg)
    (shr-render-region (point-min) (point-max))
    (buffer-string)))

(defun echat-pidgin--wrote (pidgin buffer sender msg flags)
  (let ((me (oref pidgin me))
	(msg (echat-pidgin--html-rendering msg)))
    ;; WORKAROUND: flags set to 1 seems to be me
    (when (= flags 1) (setq sender me))
    (unless (eq (window-buffer (selected-window)) buffer)
      (when-let* ((echat-buffer (echat-find-echat-buffer buffer))
		  (unread-count (oref echat-buffer unread-count)))
	(oset echat-buffer unread-p t)
	(oset echat-buffer unread-count (incf unread-count))))
    (with-current-buffer buffer
      (echat-ui-insert-msg pidgin sender me msg))))

(defun echat-pidgin--im-wrote (account sender msg id flags)
  (when-let* ((pidgin (echat-pidgin--find-by-id account))
	      (alias (echat-pidgin--buddy-alias pidgin sender))
	      (buffer (echat-pidgin--im-buffer pidgin alias id)))
    (echat-pidgin--wrote pidgin buffer alias msg flags)))

(defun echat-pidgin--chat-wrote (account sender msg id flags)
  (when-let* ((pidgin (echat-pidgin--find-by-id account))
	      (alias (echat-pidgin--buddy-alias pidgin sender))
	      (buffer (echat-pidgin--chat-buffer pidgin id)))
    (echat-pidgin--wrote pidgin buffer alias msg flags)))

;; Chats

(defun echat-pidgin--chat-set-name (chat name)
  (oset chat name name))

(defun echat-pidgin--chat-fetch-infos (id account)
  (when-let ((pidgin (echat-pidgin--find-by-id account)))
    (let ((chat (echat-pidgin-chat :id id))
	  (chats (oref pidgin chats)))
      (pidgin--call-method-async "PurpleChatGetName"
				 (apply-partially #'echat-pidgin--chat-set-name chat)
				 :int32 id)
      (add-to-list 'chats chat)
      (oset pidgin chats chats))))

(defun echat-pidgin--add-chat (id chat-p)
  (unless (zerop chat-p)
    (pidgin--call-method-async "PurpleChatGetAccount"
			       (apply-partially #'echat-pidgin--chat-fetch-infos id)
			       :int32 id)))

(defun echat-pidgin--next-chat (id)
  (unless (zerop id)
    (pidgin--call-method-async "PurpleBlistNodeIsChat"
			       (apply-partially #'echat-pidgin--add-chat id)
			       :int32 id)
    (pidgin--call-method-async "PurpleBlistNodeNext" #'echat-pidgin--next-chat
			       :int32 id :int32 0)))

(defun echat-pidgin--get-chats ()
  (pidgin--call-method-async "PurpleBlistGetRoot" #'echat-pidgin--next-chat))

;; Buddies

(defun echat-pidgin--buddy-set-alias (buddy alias)
  (oset buddy alias alias))

(defun echat-pidgin--buddy-set-name (buddy name)
  (oset buddy name name)
  (pidgin--call-method-async "PurpleBuddyGetAlias"
			     (apply-partially #'echat-pidgin--buddy-set-alias buddy)
			     :int32 (oref buddy id)))

(defun echat-pidgin--buddy-fetch-infos (buddy)
  (pidgin--call-method-async "PurpleBuddyGetName"
			     (apply-partially #'echat-pidgin--buddy-set-name buddy)
			     :int32 (oref buddy id)))

(defun echat-pidgin--get-buddies (pidgin)
  (oset pidgin buddies nil)
  (let (buddies)
    (dolist (id (pidgin--call-method "PurpleFindBuddies" :int32 (oref pidgin id)
				     :string ""))
      (let ((buddy (echat-pidgin-buddy :id id)))
	(echat-pidgin--buddy-fetch-infos buddy)
	(add-to-list 'buddies buddy)))
    (oset pidgin buddies buddies)))

;; Accounts

(defun echat-pidgin--account-signed-on (id)
  (when-let ((pidgin (echat-pidgin--find-by-id id)))
    (echat-pidgin--get-buddies pidgin)
    (oset pidgin active-p t)))

(defun echat-pidgin--account-signed-off (id)
  (when-let ((pidgin (echat-pidgin--find-by-id id)))
    (oset pidgin active-p nil)))

(defun echat-pidgin--get-accounts ()
  (dolist (id (pidgin--call-method "PurpleAccountsGetAll"))
    (let* ((protocol (pidgin--call-method "PurpleAccountGetProtocolName" :int32 id))
	   (username (pidgin--call-method "PurpleAccountGetUsername" :int32 id))
	   (me (pidgin--call-method "PurpleAccountGetAlias" :int32 id))
	   (alias (assoc-default username echat-pidgin-name-alias))
	   (face (cond ((string= protocol "Facebook") 'echat-facebook-face)
		       ((string= protocol "Slack") 'echat-slack-face)
		       ((string= protocol "IRC") 'echat-irc-face)
		       (t 'default)))
	   (pidgin (echat-pidgin :name (if alias alias username)
				 :face face
				 :id id
				 :type protocol
				 :me (if (string= me "") username me))))
      (add-to-list 'echats pidgin t))))

;; Signals

(defun echat-pidgin--unregister-signals ()
  (dolist (obj echat-pidgin--dbus-objects)
    (dbus-unregister-object obj))
  (setq echat-pidgin--dbus-objects nil))

(defun echat-pidgin--register-signals ()
  (dolist (sig echat-pidgin--signals)
    (let ((object (dbus-register-signal :session pidgin-dbus-service
					pidgin-dbus-object pidgin-dbus-interface
					(car sig) (cdr sig))))
      (add-to-list 'echat-pidgin--dbus-objects object))))

;;; External Functions

(cl-defmethod echat-mark-buffer-as-read ((pidgin echat-pidgin) buffer))

(cl-defmethod echat-do-search ((pidgin echat-pidgin))
  )

(cl-defmethod echat-do-group-select ((pidgin echat-pidgin))
  )

(cl-defmethod echat-do-channel-select ((pidgin echat-pidgin))
  (let* ((chats (oref pidgin chats))
	 (name (completing-read "Chat: " (eieio-mapcar chats name))))
    (if-let ((buffer (echat-pidgin--find-buffer pidgin name)))
	(echat-display-buffer buffer)
      (let* ((id (pidgin--call-method "PurpleConversationNew" :int32 2
				      :int32 (oref pidgin id) :string name))
	     (buffer (echat-pidgin--chat-buffer pidgin id)))
	(echat-display-buffer buffer)))))

(cl-defmethod echat-do-im-select ((pidgin echat-pidgin))
  (let* ((buddies (oref pidgin buddies))
	 (alias (completing-read "IM: " (eieio-mapcar buddies alias)))
	 (buddy (cl-find-if (lambda (b) (string= alias (oref b alias))) buddies))
	 (name (oref buddy name)))
    (if-let ((buffer (echat-pidgin--find-buffer pidgin name)))
	(echat-display-buffer buffer)
      (let* ((id (pidgin--call-method "PurpleConversationNew" :int32 1
				      :int32 (oref pidgin id) :string name))
	     (buffer (echat-pidgin--im-buffer pidgin alias id)))
	(echat-display-buffer buffer)))))

(cl-defmethod echat-do-start ((pidgin echat-pidgin))
  (pidgin--call-method "PurpleAccountSetEnabled" :int32 (oref pidgin id)
		       :string "gtk-gaim" :int32 1))

(cl-defmethod echat-do-quit ((pidgin echat-pidgin))
  (pidgin--call-method "PurpleAccountSetEnabled" :int32 (oref pidgin id)
		       :string "gtk-gaim" :int32 0))

(defun echat-pidgin-exit ()
  (interactive)
  (echat-pidgin--unregister-signals)
  (dolist (echat echats)
    (when (echat-pidgin-p echat)
      (with-slots (name active-p) echat
	(when active-p (echat-quit name))
	(setq echats (cl-delete echat echats))))))

(defun echat-pidgin-init ()
  (interactive)
  (echat-pidgin-exit)
  (echat-pidgin--get-accounts)
  (echat-pidgin--get-chats)
  (echat-pidgin--register-signals))

(provide 'echat-pidgin)
