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

;;; Internal Functions

(defun echat-slack--room-display (slack room team)
  (if-let ((buffer (slack-buffer-find 'slack-message-buffer team room)))
      (slack-buffer-display buffer)
    (slack-room-clear-messages room)
    (slack-conversations-view
     room team
     :after-success #'(lambda (messages cursor)
			(slack-room-set-messages room messages team)
			(let ((buffer (slack-create-message-buffer room cursor team)))
			  (echat-add-buffer slack (slack-buffer-buffer buffer))
                          (slack-buffer-display buffer))))))

(defun echat-slack--kill-buffers (team)
  (slack-team-kill-buffers team)
  (let ((remaining-buffers (list (slack-log-buffer-name team)
				 (slack-event-log-buffer-name team))))
    (dolist (buffer (mapcar #'get-buffer remaining-buffers))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))))

;;; External Functions

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
	 (channel (slack-select-from-list ((slack-group-names team)
					   "Group: "))))
    (echat-slack--room-display slack group team)))

(cl-defmethod echat-do-channel-select ((slack echat-slack))
  (let* ((team (oref slack team))
	 (channel (slack-select-from-list ((slack-channel-names team)
					   "Channel: "))))
    (echat-slack--room-display slack channel team)))

(cl-defmethod echat-do-im-select ((slack echat-slack))
  (let* ((team (oref slack team))
	 (im (slack-select-from-list ((slack-im-names team)
				      "IM: "))))
    (echat-slack--room-display slack im team)))

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
	      (slack (echat-slack :name name
				  :team team)))
    (add-to-list 'echats slack t)))

(provide 'echat-slack)
