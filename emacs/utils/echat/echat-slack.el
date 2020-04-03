;;; echat-slack.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2020-03-26

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

(require 'slack)

;;; Class

(defclass echat-slack (echat)
  ((team :initarg :team :type slack-team)))

;;; Faces

(defface echat-slack-face
  '((((class color) (background light)) :foreground "purple4")
    (((class color) (background  dark)) :foreground "purple2"))
  "Face for echat slack"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-slack--find-by-team (team)
  (cl-find-if (lambda (echat)
		(and (echat-slack-p echat)
		     (oref echat active-p)
		     (eq  (oref echat team) team)))
	      echats))

(defun echat-slack--find-buffer (slack name)
  (cl-find-if (lambda (echat-buffer)
		(let ((buffer (oref echat-buffer buffer)))
		  (when (or (not buffer) (buffer-live-p buffer))
		    (string= (oref echat-buffer name) name))))
	      (oref slack buffers)))

(defun echat-slack--room-query (slack echat-buffer room name)
  (slack-room-clear-messages room)
  (let* ((team (oref slack team))
	 (after-success `(lambda (messages cursor)
			   (slack-room-set-messages ,room messages ,team)
			   (when-let* ((slack-buffer (slack-create-message-buffer
						      ,room cursor ,team))
				       (buffer (slack-buffer-buffer slack-buffer)))
			     (oset ,echat-buffer buffer buffer)
			     (echat-display-buffer buffer)))))
    (slack-conversations-view room team :after-success after-success)))

(defun echat-slack--room-display (slack room name)
  (let ((echat-buffer (echat-slack--find-buffer slack name)))
    (unless echat-buffer
      (setq echat-buffer (echat-add-buffer slack name nil
					   #'echat-slack--room-display
					   (list slack room name))))
    (let ((buffer (oref echat-buffer buffer)))
      (if (buffer-live-p buffer)
	  (echat-display-buffer buffer)
	(echat-slack--room-query slack echat-buffer room name)))))

(defun echat-slack--kill-buffers (team)
  (slack-team-kill-buffers team)
  (let ((remaining-buffers (list (slack-log-buffer-name team)
				 (slack-event-log-buffer-name team))))
    (dolist (buffer (mapcar #'get-buffer remaining-buffers))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))))

(defun echat-slack--select (team rooms prompt)
  (let* ((names (mapcar (lambda (room) (slack-room-name room team))
			rooms))
	 (name (completing-read prompt names)))
    (cl-find-if (lambda (room) (string= (slack-room-name room team) name))
		rooms)))

(defun echat-slack--unread (slack id unread-p unread-count func)
  (when-let* ((team (oref slack team))
	      (rooms (funcall func team))
	      (room (cl-find-if (lambda (r) (string= (oref r id) id)) rooms))
	      (name (slack-room-name room team)))
    (let ((echat-buffer (echat-slack--find-buffer slack name)))
      (unless echat-buffer
	(setq echat-buffer (echat-add-buffer slack name nil
					     #'echat-slack--room-display
					     (list slack room name))))
      (oset echat-buffer unread-p unread-p)
      (oset echat-buffer unread-count unread-count))))

(defun echat-slack--handle-counts-update (team counts)
  (when-let ((slack (echat-slack--find-by-team team)))
    (with-slots (channels ims) counts
      (pcase-dolist (`(,rooms . ,func) (list (cons channels 'slack-team-channels)
					     (cons ims 'slack-team-ims)))
	(dolist (room rooms)
	  (with-slots (id has-unreads mention-count) room
	    (when has-unreads
	      (echat-slack--unread slack id has-unreads mention-count func))))))))

(defun echat-slack--handle-push-message (team room)
  (when-let* ((slack (echat-slack--find-by-team team))
	      (name (slack-room-name room team)))
    (if-let ((echat-buffer (echat-slack--find-buffer slack name)))
	(with-slots (buffer unread-count) echat-buffer
	  (unless (and buffer (get-buffer-window-list buffer))
	    (oset echat-buffer unread-p t)
	    (oset echat-buffer unread-count (incf unread-count))))
      (setq echat-buffer (echat-add-buffer slack name nil
					   #'echat-slack--room-display
					   (list slack room name)))
      (oset echat-buffer unread-p t)
      (oset echat-buffer unread-count 1))))

;;; Slack Activity

(cl-defmethod echat-slack--message-header ((this slack-message) team)
  (when-let* ((slack (echat-slack--find-by-team team))
	      (face (oref slack face))
	      (self-name (slack-user-name (oref team self-id) team))
	      (name (slack-message-sender-name this team)))
    (let* ((edited-at (slack-format-ts (slack-message-edited-at this)))
           (deleted-at (slack-format-ts (oref this deleted-at)))
	   (me (string= self-name name))
	   (nick-emoji (when-let* ((id (slack-message-sender-id this))
				   (user (slack-user--find id team))
				   (profile (plist-get user :profile)))
			 (plist-get profile :status_emoji)))
           (nick-icon (when-let ((image (slack-message-profile-image this team)))
			(propertize "image" 'display image 'face
				    'slack-profile-image-face)))
	   (nick-face (if me 'echat-irc-nick-face face))
	   (nick-str (propertize name 'face `(bold ,nick-face)))
	   (current-time (current-time))
	   (ts (format-time-string lui-time-stamp-format
				   current-time
                                   lui-time-stamp-zone))
	   (ts-str (propertize ts 'face 'lui-time-stamp-face))
	   (spaces (- (/ lui-fill-column 2) (/ (length ts) 2)))
	   (fmt (format "\n%%-%ds%%s%%%ds\n" spaces spaces)))
      (format fmt (if (not me)
		      (concat (when nick-icon (concat nick-icon " "))
			      nick-str
			      (when nick-emoji (concat " " nick-emoji)))
		    " ")
	      ts-str
	      (if me
		  (concat (when nick-emoji (concat nick-emoji " "))
			  nick-str
			  (when nick-icon (concat " " nick-icon)))
		" ")))))
(advice-add 'slack-message-header :override #'echat-slack--message-header)

(cl-defmethod echat-slack--counts-update ((team slack-team))
  (let ((after-success `(lambda (counts)
			  (echat-slack--handle-counts-update ,team counts)
                          (oset ,team counts counts))))
    (slack-client-counts team after-success)))
(advice-add 'slack-counts-update :override #'echat-slack--counts-update)

(cl-defmethod echat-slack--push-message ((this slack-room) message team)
  (echat-slack--handle-push-message team this))
(advice-add 'slack-room-push-message :after #'echat-slack--push-message)

;;; External Functions

(cl-defmethod echat-do-search ((slack echat-slack))
  (let* ((team (oref slack team))
	 (query (read-string "Query: "))
	 (search (slack-search-result :sort "timestamp"
				      :sort-dir "desc"
				      :query query))
	 (after-success `(lambda ()
			   (let ((buffer (slack-create-search-result-buffer
					  ,search ,team)))
                             (slack-buffer-display buffer)))))
    (slack-search-request search after-success team)))

(cl-defmethod echat-do-group-select ((slack echat-slack))
  (with-slots (name face team) slack
    (let* ((groups (slack-team-groups team))
	   (prompt (format "Group (%s): " (propertize name 'face face)))
	   (group (echat-slack--select team groups prompt))
	   (name (slack-room-name group team)))
      (echat-slack--room-display slack group name))))

(cl-defmethod echat-do-channel-select ((slack echat-slack))
  (with-slots (name face team) slack
    (let* ((channels (slack-team-channels team))
	   (prompt (format "Channel (%s): " (propertize name 'face face)))
	   (channel (echat-slack--select team channels prompt))
	   (name (concat "#" (slack-room-name channel team))))
      (echat-slack--room-display slack channel name))))

(cl-defmethod echat-do-im-select ((slack echat-slack))
  (with-slots (name face team) slack
    (let* ((ims (cl-remove-if-not (lambda (im) (oref im is-open))
				  (slack-team-ims team)))
	   (prompt (format "IM (%s): " (propertize name 'face face)))
	   (im (echat-slack--select team ims prompt))
	   (name (slack-room-name im team)))
      (echat-slack--room-display slack im name))))

(cl-defmethod echat-do-start ((slack echat-slack))
  (slack-start (oref slack team)))

(cl-defmethod echat-do-quit ((slack echat-slack))
  (let ((team (oref slack team)))
    (slack-ws--close (oref team ws) team t)
    (echat-slack--kill-buffers team)))

(defun echat-register-slack (&rest plist)
  (apply #'slack-register-team plist)
  (when-let* ((name (plist-get plist :name))
	      (token (plist-get plist :token))
	      (team (slack-team-find-by-token token))
	      (slack (echat-slack :name  name
				  :face  'echat-slack-face
				  :team  team)))
    (add-to-list 'echats slack t)))

(provide 'echat-slack)
