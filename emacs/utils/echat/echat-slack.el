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
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for echat slack"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-slack--buffer-display (slack name buffer)
  (echat-add-buffer slack name buffer)
  (echat-display-buffer buffer))

(defun echat-slack--room-display (slack team room name)
  (if-let ((buffer (slack-buffer-find 'slack-message-buffer team room)))
      (echat-slack--buffer-display slack name buffer)
    (slack-room-clear-messages room)
    (slack-conversations-view
     room team
     :after-success (lambda (messages cursor)
		      (slack-room-set-messages room messages team)
		      (let* ((slack-buffer (slack-create-message-buffer
					    room cursor team))
			     (buffer (slack-buffer-buffer slack-buffer)))
			;; WORKAROUND:
			;; For unknown reasons sometimes the buffer is not
			;; "ready", put `message' here seeems to workaround
			;; this issue.
			(message nil)
			(echat-slack--buffer-display slack name buffer))))))

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

;;; External Functions

(cl-defmethod echat-unread-queries ((slack echat-slack))
  (let* ((team (oref slack team))
	 (ims (slack-team-ims team))
	 (channels (slack-team-channels team))
	 (groups (slack-team-groups team))
	 data)
    (dolist (room (apply #'append (list ims channels groups)))
      (when-let* ((unread (slack-room-has-unread-p room team))
		  (count (slack-room-mention-count room team))
		  (name (if (slack-channel-p room)
			    (concat "#" (slack-room-name room team))
			  (slack-room-name room team)))
		  (query (apply-partially #'echat-slack--room-display
					  slack team room name))
		  (plist (list :name name :echat slack
			       :count count :query query)))
	(add-to-list 'data plist t)))
    data))

(cl-defmethod echat-do-search ((slack echat-slack))
  (let* ((team (oref slack team))
	 (query (read-string "Query: "))
	 (search (slack-search-result :sort "timestamp"
				      :sort-dir "desc"
				      :query query))
	 (after-success (lambda ()
			  (let ((buffer (slack-create-search-result-buffer
					 search team)))
                            (slack-buffer-display buffer)))))
    (slack-search-request search after-success team)))

(cl-defmethod echat-do-group-select ((slack echat-slack))
  (let* ((team (oref slack team))
	 (groups (slack-team-groups team))
	 (face (oref slack face))
	 (prompt (format "Group (%s): " (propertize (oref slack name)
						    'face face)))
	 (group (echat-slack--select team groups prompt))
	 (name (slack-room-name group team)))
    (echat-slack--room-display slack team group name)))

(cl-defmethod echat-do-channel-select ((slack echat-slack))
  (let* ((team (oref slack team))
	 (channels (slack-team-channels team))
	 (face (oref slack face))
	 (prompt (format "Channel (%s): " (propertize (oref slack name)
						      'face face)))
	 (channel (echat-slack--select team channels prompt))
	 (name (concat "#" (slack-room-name channel team))))
    (echat-slack--room-display slack team channel name)))

(cl-defmethod echat-do-im-select ((slack echat-slack))
  (let* ((team (oref slack team))
	 (ims (cl-remove-if-not (lambda (im) (oref im is-open))
				(slack-team-ims team)))
	 (face (oref slack face))
	 (prompt (format "IM (%s): " (propertize (oref slack name)
						 'face face)))
	 (im (echat-slack--select team ims prompt))
	 (name (slack-room-name im team)))
    (echat-slack--room-display slack team im name)))

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
