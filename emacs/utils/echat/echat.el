;;; echat.el --- XXXX

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

(require 'eieio)

;;; Class

(defclass echat ()
  ((name     :initarg :name     :initform ""  :type string)
   (face     :initarg :face     :initform nil)
   (active-p :initarg :active-p :initform nil :type boolean)
   (buffers  :initarg :buffers  :initform nil)))

;;; Groups

(defgroup echat nil
  "echat group"
  :group 'applications)

(defgroup echat-faces nil
  "Faces used by echat"
  :group 'echat
  :group 'faces)

;;; Chat Supported

(require 'echat-slack)

;;; External Variables

(defvar echats nil)

;;; Generic Functions

(cl-defgeneric echat-add-buffer (echat name buffer)
  "Add buffer to buffer list of echat object"
  (let ((buffers (oref echat buffers)))
    (when (bufferp buffer)
      (add-to-list 'buffers (cons name buffer) t)
      (oset echat buffers buffers))))

(cl-defgeneric echat-unread-queries (obj)
  "Return a list of plist with the following keys:
- name:  Channel, Group or IM name
- echat: echat object
- count: number of unread message
- query: function used to jump to this unread")

(cl-defgeneric echat-do-search (obj)
  "Search echat object")

(cl-defgeneric echat-do-channel-select (obj)
  "Select Channel echat object")

(cl-defgeneric echat-do-group-select (obj)
  "Select Group echat object")

(cl-defgeneric echat-do-im-select (obj)
  "Select IM echat object")

(cl-defgeneric echat-do-start (obj)
  "Start echat object")

(cl-defgeneric echat-do-quit (obj)
  "Quit echat object")

;;; Internal Functions

(defun echat--find-by-name (name)
  (cl-find-if (lambda (echat)
		(string= (oref echat name) name))
	      echats))

(defun echat--prompt-unread (prompt)
  (let* ((data (delq nil (mapcar #'echat-unread-queries echats)))
	 (unreads (apply #'append data))
	 (collection (mapcar (lambda (unread)
			       (let ((name (plist-get unread :name))
				     (echat (plist-get unread :echat)))
				 (propertize name 'face (oref echat face))))
			     unreads))
	 (name (completing-read prompt collection)))
    (cl-find-if (lambda (unread) (string= (plist-get unread :name) name))
		unreads)))

(defun echat--prompt-inactive (prompt)
  (let* ((inactives (cl-remove-if (lambda (echat) (oref echat active-p))
				  echats))
	 (collection (mapcar (lambda (echat)
			       (with-slots (name face) echat
				 (propertize name 'face face)))
			     inactives)))
    (completing-read prompt collection)))

(defun echat--prompt-active (prompt)
  (let* ((actives (cl-remove-if-not (lambda (echat) (oref echat active-p))
				    echats))
	 (collection (mapcar (lambda (echat)
			       (with-slots (name face) echat
				 (propertize name 'face face)))
			     actives)))
    (completing-read prompt collection)))

(defun echat--prompt-buffers (prompt)
  (let (collection)
    (dolist (echat echats)
      (let ((face (oref echat face)))
	(pcase-dolist (`(,name . ,buffer) (oref echat buffers))
	  (when (buffer-live-p buffer)
	    (add-to-list 'collection (cons (propertize name 'face face)
					   buffer))))))
    (let ((name (completing-read prompt (mapcar #'car collection))))
      (cdr (assq name collection)))))

;;; External Functions

(defun echat-display-buffer (buffer)
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)))

(defun echat-search (name)
  (interactive (list (echat--prompt-active "Search: ")))
  (echat-do-search (echat--find-by-name name)))

(defun echat-channel-select (name)
  (interactive (list (echat--prompt-active "Channel: ")))
  (echat-do-channel-select (echat--find-by-name name)))

(defun echat-group-select (name)
  (interactive (list (echat--prompt-active "Group: ")))
  (echat-do-group-select (echat--find-by-name name)))

(defun echat-im-select (name)
  (interactive (list (echat--prompt-active "IM: ")))
  (echat-do-im-select (echat--find-by-name name)))

(defun echat-unread (plist)
  (interactive (list (echat--prompt-unread "Unread: ")))
  (funcall (plist-get plist :query)))

(defun echat-jump (buffer)
  (interactive (list (echat--prompt-buffers "Jump: ")))
  (echat-display-buffer buffer))

(defun echat-start (name)
  (interactive (list (echat--prompt-inactive "Start: ")))
  (let ((echat (echat--find-by-name name)))
    (echat-do-start echat)
    (oset echat active-p t)))

(defun echat-quit (name)
  (interactive (list (echat--prompt-active "Quit: ")))
  (let ((echat (echat--find-by-name name)))
    (echat-do-quit echat)
    (oset echat active-p nil)
    (dolist (buffer (oref echat buffers))
      (when (buffer-live-p buffer)
	(kill-buffer buffer)))
    (oset echat buffers nil)))

(provide 'echat)
