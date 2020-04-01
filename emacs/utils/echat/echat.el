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
  ((name         :initarg :name         :initform ""  :type string)
   (face         :initarg :face         :initform nil)
   (active-p     :initarg :active-p     :initform nil :type boolean)
   (buffers      :initarg :buffers      :initform nil)))

(defclass echat-buffer ()
  ((name         :initarg :name         :initform ""  :type string)
   (buffer       :initarg :buffer       :initform nil)
   (unread-p     :initarg :unread-p     :initform nil :type boolean)
   (unread-count :initarg :unread-count :initform 0   :type number)
   (query        :initarg :query        :initform nil)
   (query-args   :initarg :query-args   :initform nil)))

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
(require 'echat-irc)
(require 'echat-facebook)

;;; External Variables

(defvar echats nil)

;;; Generic Functions

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

(defun echat--mark-buffer-as-read (buffer)
  (when-let ((echat-buffer (echat-find-echat-buffer buffer)))
    (oset echat-buffer unread-p nil)
    (oset echat-buffer unread-count 0)))

(defun echat--find-by-name (name)
  (cl-find-if (lambda (echat)
		(string= (oref echat name) name))
	      echats))

(defun echat--remove-buffers-killed (echat)
  (let (buffers)
    (dolist (echat-buffer (oref echat buffers))
      (let ((buffer (oref echat-buffer buffer)))
	(when (or (not buffer) (buffer-live-p buffer))
	  (add-to-list 'buffers echat-buffer t))))
    (oset echat buffers buffers)))

(defun echat--prompt (prompt collection)
  (completing-read prompt (mapcar (lambda (echat)
				    (with-slots (name face) echat
				      (propertize name 'face face)))
				  collection)))

(defun echat--prompt-inactive (prompt)
  (echat--prompt prompt (cl-remove-if (lambda (echat) (oref echat active-p))
				      echats)))

(defun echat--prompt-active (prompt)
  (echat--prompt prompt (cl-remove-if-not (lambda (echat) (oref echat active-p))
					  echats)))

(defun echat--prompt-buffers (prompt)
  (let (collection)
    (dolist (echat echats)
      (let ((face (oref echat face)))
	(dolist (echat-buffer (oref echat buffers))
	  (with-slots (name buffer) echat-buffer
	    (when (buffer-live-p buffer)
	      (add-to-list 'collection (cons (propertize name 'face face)
					     echat-buffer)))))))
    (let ((name (completing-read prompt (mapcar #'car collection))))
      (oref (cdr (assq name collection)) buffer))))

(defun echat--prompt-unread-buffers (prompt)
  (let (collection)
    (dolist (echat echats)
      (let ((face (oref echat face)))
	(dolist (echat-buffer (oref echat buffers))
	  (with-slots (name buffer unread-p unread-count) echat-buffer
	    (when (and (or (not buffer) (buffer-live-p buffer)) unread-p)
	      (let ((str (if (zerop unread-count) name
			   (format "%s (%s)" name unread-count))))
		(add-to-list 'collection (cons (propertize str 'face face)
					       echat-buffer))))))))
    (let ((name (completing-read prompt (mapcar #'car collection))))
      (cdr (assq name collection)))))

;;; External Functions

(defun echat-find-echat-buffer (buffer)
  (catch 'found
    (dolist (echat echats)
      (dolist (echat-buffer (oref echat buffers))
	(when (eq buffer (oref echat-buffer buffer))
	  (throw 'found echat-buffer))))))

(defun echat-find-by-buffer (buffer)
  (cl-find-if (lambda (echat)
		(cl-find-if (lambda (echat-buffer)
			      (eq (oref echat-buffer buffer) buffer))
			    (oref echat buffers)))
	      echats))

(defun echat-add-buffer (echat name buffer &optional query query-args)
  (echat--remove-buffers-killed echat)
  (let ((buffers (oref echat buffers))
	(echat-buffer (echat-buffer :name name :buffer buffer
				    :query query :query-args query-args)))
    (add-to-list 'buffers echat-buffer t)
    (oset echat buffers buffers)
    echat-buffer))

(defun echat-unregister (name)
  (interactive (list (echat--prompt "Search: " echats)))
  (let ((echat (echat--find-by-name name)))
    (with-slots (name active-p) echat
      (when active-p (echat-quit name))
      (setq echats (cl-delete echat echats)))))

(defun echat-display-buffer (buffer)
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)
    (echat--mark-buffer-as-read buffer)))

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

(defun echat-unread-select (echat-buffer)
  (interactive (list (echat--prompt-unread-buffers "Unread: ")))
  (with-slots (buffer query query-args) echat-buffer
    (if (buffer-live-p buffer)
	(echat-display-buffer buffer)
      (if query
	  (apply query query-args)
	(error "No query found to retrieve this buffer")))))

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
    (dolist (echat-buffer (oref echat buffers))
      (with-slots (buffer) echat-buffer
	(when (buffer-live-p buffer)
	  (kill-buffer buffer))))
    (oset echat buffers nil)))

(provide 'echat)
