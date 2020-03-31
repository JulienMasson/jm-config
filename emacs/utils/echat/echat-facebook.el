;;; echat-facebook.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2020-03-31

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

(require 'echat-bitlbee)
(require 'echat-irc)

;;; Class

(defclass echat-facebook (echat-irc-object)
  ((user            :initarg :user            :type string)
   (password        :initarg :password        :type string)
   (work            :initarg :work            :initform nil :type boolean)
   (bitlbee         :initarg :bitlbee         :type echat-bitlbee)
   (bitlbee-channel :initarg :bitlbee-channel)))

;;; Faces

(defface echat-facebook-face
  '((((class color) (background light)) :foreground "DodgerBlue2")
    (((class color) (background  dark)) :foreground "LightBlue2"))
  "Face for echat facebook"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-facebook--add-user (facebook user)
  (let ((candidates (oref facebook candidates)))
    (add-to-list 'candidates user t)
    (oset facebook candidates candidates)))

(defun echat-facebook--connected (facebook)
  (circe-command-SAY "blist all"))

(defun echat-facebook--connect (facebook)
  (with-slots (user password work) facebook
    (circe-command-SAY (format "account add facebook %s %s" user password))
    (when work (circe-command-SAY "account facebook set work true"))
    (circe-command-SAY "account facebook set group_chat_open all")
    (circe-command-SAY "account facebook on")))

;;; External Functions

(cl-defmethod echat-irc-new-buffer ((facebook echat-facebook) name)
  (cond ((eq major-mode 'circe-channel-mode)
	 (oset echat bitlbee-channel (current-buffer))
	 (echat-facebook--connect facebook))
	((eq major-mode 'circe-query-mode)
	 (echat-add-buffer facebook name (current-buffer)))))

(cl-defmethod echat-irc-new-channel-msg ((facebook echat-facebook) msg)
  (cond ((string-match "^<root> \\([[:graph:]]*\\).*\\(Offline\\|Online\\)$" msg)
	 (echat-facebook--add-user facebook (match-string 1 msg)))
	((string-match "^<root> facebook - Logging in: Logged in$" msg)
	 (echat-facebook--connected facebook))))

(cl-defmethod echat-get-unread ((facebook echat-facebook))
  )

(cl-defmethod echat-do-search ((facebook echat-facebook))
  (error "Operation not supported"))

(cl-defmethod echat-do-group-select ((facebook echat-facebook))
  (error "Operation not supported"))

(cl-defmethod echat-do-channel-select ((facebook echat-facebook))
  (error "Operation not supported"))

(cl-defmethod echat-do-im-select ((facebook echat-facebook))
  (with-current-buffer (oref facebook bitlbee-channel)
    (let* ((user (completing-read "IM: " (oref facebook candidates)))
	   (query-buffer (circe-server-get-or-create-chat-buffer
			  user 'circe-query-mode)))
      (echat-display-buffer query-buffer))))

(cl-defmethod echat-do-start :before ((facebook echat-facebook))
  (echat-do-start (oref facebook bitlbee)))

(cl-defmethod echat-do-quit :before ((facebook echat-facebook))
  (echat-do-quit (oref facebook bitlbee))
  (with-slots (bitlbee-channel) facebook
    (when (buffer-live-p bitlbee-channel)
      (kill-buffer bitlbee-channel))))

(defun echat-register-facebook (&rest plist)
  (let* ((name (plist-get plist :name))
	 (user (plist-get plist :user))
	 (password (plist-get plist :password))
	 (work (plist-get plist :work))
	 (bitlbee (echat-bitlbee-create name "-n" "-D" "-v"))
	 (circe-options (list :host "localhost"
			      :port (oref bitlbee port)))
	 (facebook (echat-facebook :name name
				   :face 'echat-facebook-face
				   :circe-options circe-options
				   :user user
				   :password password
				   :work work
				   :bitlbee bitlbee)))
    (add-to-list 'echats facebook t)))

(provide 'echat-facebook)
