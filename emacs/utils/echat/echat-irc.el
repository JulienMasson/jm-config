;;; echat-irc.el --- XXXX

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

(require 'circe)

;;; Class

(defclass echat-irc-object (echat)
  ((circe-options :initarg :circe-options :initform nil)
   (server-buffer :initarg :server-buffer :initform nil)
   (candidates    :initarg :candidates    :initform nil))
  :abstract t)

(defclass echat-irc-candidate ()
  ((buffer     :initarg :buffer     :initform nil)
   (users      :initarg :users      :initform nil)))

(defclass echat-irc (echat-irc-object) ())

;;; Faces

(defface echat-irc-face
  '((((class color) (background light)) :foreground "CadetBlue4")
    (((class color) (background  dark)) :foreground "CadetBlue1"))
  "Face for echat irc"
  :group 'echat-faces)

;;; Internal Functions

(defun echat-irc--channel-buffers (irc)
  (cl-remove-if-not (lambda (echat-buffer)
		      (with-slots (buffer) echat-buffer
			(when (buffer-live-p buffer)
			  (with-current-buffer buffer
			    (eq major-mode 'circe-channel-mode)))))
		    (oref irc buffers)))

(defun echat-irc--find-by-server-buffer (server-buffer)
  (cl-find-if (lambda (echat)
		(and (object-of-class-p echat 'echat-irc-object)
		     (eq (oref echat server-buffer) server-buffer)))
	      echats))

(defun echat-irc--add-users (new-users)
  (when-let* ((buffer (current-buffer))
	      (irc (echat--find-by-buffer (current-buffer))))
    (if-let* ((candidates (oref irc candidates))
	      (candidate (cl-find-if (lambda (candidate)
				       (eq buffer (oref candidate buffer)))
				     candidates)))
	(with-slots (users) candidate
	  (oset candidate users (append users new-users)))
      (add-to-list 'candidates (echat-irc-candidate :buffer buffer
						    :users new-users))
      (oset irc candidates candidates))))

(defun echat-irc--remove-user (user)
  (when-let* ((buffer (current-buffer))
	      (irc (echat--find-by-buffer (current-buffer)))
	      (candidates (oref irc candidates))
	      (candidate (cl-find-if (lambda (candidate)
				       (eq buffer (oref candidate buffer)))
				     candidates)))
    (oset candidate users (cl-remove user (oref candidate users)
				     :test #'string=))))

;;; Watching Circe Activity

(defun echat-irc--watch-create-chat-buffer (orig-func &rest args)
  (with-current-buffer (apply orig-func args)
    (when-let* ((server-buffer (circe-server-buffer))
		(echat (echat-irc--find-by-server-buffer server-buffer)))
      (echat-irc-new-buffer echat (car args)))
    (current-buffer)))
(advice-add 'circe-server-create-chat-buffer :around
	    #'echat-irc--watch-create-chat-buffer)

(defun echat-irc--watch-display (orig-func &rest args)
  (apply orig-func args)
  (when (eq major-mode 'circe-channel-mode)
    (when-let* ((server-buffer (circe-server-buffer))
		(echat (echat-irc--find-by-server-buffer server-buffer))
		(msg (substring-no-properties (apply #'lui-format args))))
      (echat-irc-new-channel-msg echat msg))))
(advice-add 'circe-display :around #'echat-irc--watch-display)

;;; External Functions

(cl-defmethod echat-irc-new-buffer ((irc echat-irc) name)
  (when (derived-mode-p 'circe-chat-mode)
    (echat-add-buffer irc name (current-buffer))))

(cl-defmethod echat-irc-new-channel-msg ((irc echat-irc) msg)
  (cond ((string-match "^\*\*\* Names: \\(.*\\)" msg)
	 (echat-irc--add-users (split-string (match-string 1 msg))))
	((string-match "^\*\*\* Join: \\([[:graph:]]*\\)" msg)
	 (echat-irc--add-users (list (match-string 1 msg))))
	((string-match "^\*\*\* \\(Quit\\|Part\\): \\([[:graph:]]*\\)" msg)
	 (echat-irc--remove-user (match-string 2 msg)))))

(cl-defmethod echat-get-unread ((irc echat-irc-object))
  )

(cl-defmethod echat-do-search ((irc echat-irc-object))
  (error "Operation not supported"))

(cl-defmethod echat-do-group-select ((irc echat-irc-object))
  (error "Operation not supported"))

(cl-defmethod echat-do-channel-select ((irc echat-irc-object))
  (with-current-buffer (oref irc server-buffer)
    (let* ((channel (read-string "Channel: "))
	   (channel-buffer (circe-server-get-or-create-chat-buffer
			    channel 'circe-channel-mode)))
      (irc-send-JOIN (circe-server-process) channel)
      (echat-add-buffer irc channel channel-buffer)
      (echat-display-buffer channel-buffer))))

(cl-defmethod echat-do-im-select ((irc echat-irc-object))
  (let* ((buffers (echat-irc--channel-buffers irc))
	 (buffers-name (mapcar (lambda (buffer) (oref buffer name)) buffers))
	 (name (completing-read "Select Channel: " buffers-name))
	 (echat-buffer (cl-find-if (lambda (buffer)
				     (string= name (oref buffer name)))
				   buffers))
	 (candidate (cl-find-if (lambda (candidate)
				  (eq (oref echat-buffer buffer)
				      (oref candidate buffer)))
				(oref irc candidates)))
	 (user (completing-read "IM: " (if candidate (oref candidate users)))))
    (with-current-buffer (oref candidate buffer)
      (let ((query-buffer (circe-server-get-or-create-chat-buffer
			   user 'circe-query-mode)))
	(echat-display-buffer query-buffer)))))

(cl-defmethod echat-do-start ((irc echat-irc-object))
  (let* ((buffer-name (format "*IRC: %s*" (oref irc name)))
	 (buffer (get-buffer-create buffer-name)))
    (oset irc server-buffer buffer)
    (oset irc candidates nil)
    (with-current-buffer buffer
      (circe-server-mode)
      (circe--server-set-variables (oref irc circe-options))
      (circe-reconnect))))

(cl-defmethod echat-do-quit ((irc echat-irc-object))
  (let ((server-buffer (oref irc server-buffer)))
    (when (buffer-live-p server-buffer)
      (kill-buffer server-buffer))
    (oset irc server-buffer nil)))

(defun echat-register-irc (name &rest options)
  (let ((irc (echat-irc :name name
			:face 'echat-irc-face
			:circe-options options)))
    (add-to-list 'echats irc t)))

(provide 'echat-irc)
