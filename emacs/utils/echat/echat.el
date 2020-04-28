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
   (mute-p       :initarg :mute-p       :initform nil :type boolean)
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

(cl-defgeneric echat-mark-buffer-as-read (obj buffer)
  "Mark buffer as read with echat object")

(cl-defgeneric echat-do-search (obj)
  "Search with echat object")

(cl-defgeneric echat-do-channel-select (obj)
  "Select Channel with echat object")

(cl-defgeneric echat-do-group-select (obj)
  "Select Group with echat object")

(cl-defgeneric echat-do-im-select (obj)
  "Select IM with echat object")

(cl-defgeneric echat-do-start (obj)
  "Start with echat object")

(cl-defgeneric echat-do-quit (obj)
  "Quit with echat object")

;;; Internal Functions

(defmacro eieio-mapcar (sequence slot)
  (declare (indent defun))
  `(mapcar (lambda (e) (oref e ,slot)) ,sequence))

(defun echat--mark-buffer-as-read (buffer)
  (when-let* ((echat (echat-find-by-buffer buffer))
	      (echat-buffer (echat-find-echat-buffer buffer)))
    (oset echat-buffer unread-p nil)
    (oset echat-buffer unread-count 0)
    (echat-mark-buffer-as-read echat buffer)))

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

(defun echat--prompt (prompt echats &optional all)
  (let ((collection (mapcar (lambda (echat)
			      (with-slots (name face) echat
				(propertize name 'face face)))
			    echats)))
    (when (and all (> (length collection) 1))
      (push "ALL" collection))
    (completing-read prompt collection)))

(defun echat--inactive ()
  (cl-remove-if (lambda (echat) (oref echat active-p)) echats))

(defun echat--prompt-inactive (prompt &optional all)
  (echat--prompt prompt (echat--inactive) all))

(defun echat--active ()
  (cl-remove-if-not (lambda (echat) (oref echat active-p))echats))

(defun echat--prompt-active (prompt &optional all)
  (echat--prompt prompt (echat--active) all))

(defun echat--sort-by-mute (collection)
  (cl-sort collection (lambda (mute-a mute-b)
			(or (not mute-a) mute-b))
	   :key (lambda (e) (oref (cdr e) mute-p))))

(defun echat--prompt-buffers (prompt)
  (let (collection)
    (dolist (echat echats)
      (let ((face (oref echat face)))
	(dolist (echat-buffer (oref echat buffers))
	  (with-slots (name buffer) echat-buffer
	    (when (buffer-live-p buffer)
	      (add-to-list 'collection (cons (propertize name 'face face)
					     echat-buffer)))))))
    (let* ((sorted-collection (echat--sort-by-mute collection))
	   (name (completing-read prompt (mapcar #'car sorted-collection))))
      (oref (cdr (assq name sorted-collection)) buffer))))

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
    (let* ((sorted-collection (echat--sort-by-mute collection))
	   (name (completing-read prompt (mapcar #'car sorted-collection))))
      (cdr (assq name sorted-collection)))))

(defun echat--select-window (old-fn &rest args)
  (let ((prev-buffer (current-buffer))
	next-buffer)
    (apply old-fn args)
    (setq next-buffer (current-buffer))
    ;; mark as read
    (with-current-buffer next-buffer
      (when (derived-mode-p 'lui-mode)
	(echat--mark-buffer-as-read next-buffer)))
    ;; remove unread separator
    (with-current-buffer prev-buffer
      (when (and (derived-mode-p 'lui-mode)
		 (not (eq prev-buffer next-buffer)))
	(echat-ui-remove-unread-separator prev-buffer)))))
(advice-add 'select-window :around #'echat--select-window)

;;; External Functions

(defun echat-find-echat-buffer (buffer)
  (catch 'found
    (dolist (echat echats)
      (dolist (echat-buffer (oref echat buffers))
	(when (eq buffer (oref echat-buffer buffer))
	  (throw 'found echat-buffer))))))

(defun echat-find-by-buffer (buffer)
  (catch 'found
    (dolist (echat echats)
      (dolist (echat-buffer (oref echat buffers))
	(when (eq buffer (oref echat-buffer buffer))
	  (throw 'found echat))))))

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

(defun echat-unread-select (echat-buffer)
  (interactive (list (echat--prompt-unread-buffers "Unread: ")))
  (with-slots (buffer query query-args) echat-buffer
    (if (buffer-live-p buffer)
	(echat-display-buffer buffer)
      (if query
	  (apply query query-args)
	(error "No query found to retrieve this buffer")))))

(defun echat-mute-toggle (buffer)
  (interactive (list (echat--prompt-buffers "Toggle Mute: ")))
  (let* ((echat-buffer (echat-find-echat-buffer buffer))
	 (mute-p (oref echat-buffer mute-p)))
    (oset echat-buffer mute-p (not mute-p))))

(defun echat-jump (buffer)
  (interactive (list (echat--prompt-buffers "Jump: ")))
  (echat-display-buffer buffer))

(defun echat-restart (name)
  (interactive (list (echat--prompt-active "Restart: " t)))
  (let ((actives (eieio-mapcar (echat--active) name)))
    (dolist (name (if (string= name "ALL") actives (list name)))
      (echat-quit name)
      (echat-start name))))

(defun echat-start (name)
  (interactive (list (echat--prompt-inactive "Start: " t)))
  (let ((inactives (eieio-mapcar (echat--inactive) name)))
    (dolist (name (if (string= name "ALL") inactives (list name)))
      (let ((echat (echat--find-by-name name)))
	(echat-do-start echat)))))

(defun echat-quit (name)
  (interactive (list (echat--prompt-active "Quit: " t)))
  (let ((actives (eieio-mapcar (echat--active) name)))
    (dolist (name (if (string= name "ALL") actives (list name)))
      (let ((echat (echat--find-by-name name)))
	(echat-do-quit echat)
	(oset echat active-p nil)
	(dolist (echat-buffer (oref echat buffers))
	  (with-slots (buffer) echat-buffer
	    (when (buffer-live-p buffer)
	      (kill-buffer buffer))))
	(oset echat buffers nil)))))

(provide 'echat)
