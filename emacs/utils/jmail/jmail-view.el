;;; jmail-view.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-07-12

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

(define-derived-mode jmail-view-mode text-mode
  "jmail view"
  (setq-local font-lock-defaults '(message-font-lock-keywords t))
  (toggle-read-only t))

(define-key jmail-view-mode-map "n" 'jmail-search-next)
(define-key jmail-view-mode-map "p" 'jmail-search-previous)
(define-key jmail-view-mode-map "q" 'jmail-view-quit)

;;; Internal Variables

(defconst jmail-view--buffer-name "*jmail-view*")

;;; Internal Functions

(defmacro with-jmail-view-buffer (&rest body)
  `(let ((inhibit-read-only t))
     (if (get-buffer jmail-view--buffer-name)
	 (with-current-buffer jmail-view--buffer-name
	          ,@body)
       (with-current-buffer (get-buffer-create jmail-view--buffer-name)
	 (jmail-view-mode)
	 ,@body))))

(defun jmail-view--setup-buffer (buffer)
  (with-current-buffer buffer
    (let* ((window-down (windmove-find-other-window 'down))
	   (buffer-down (window-buffer window-down))
	   (buffer-view (with-jmail-view-buffer (current-buffer))))
      (if (eq buffer-down buffer-view)
	  (switch-to-buffer-other-window buffer-view)
	(split-window-below 20)
	(windmove-down)
	(switch-to-buffer buffer-view)))))

;;; External Functions

(defun jmail-view-quit ()
  (interactive)
  (when (get-buffer jmail-view--buffer-name)
    (with-jmail-view-buffer
     (kill-buffer-and-window))))

(defun jmail-view (object buffer)
  (jmail-view--setup-buffer buffer)
  (with-jmail-view-buffer
   (let ((path (plist-get object :path)))
     (erase-buffer)
     (apply #'process-file "mu" nil t nil (list "view" "--nocolor" path))
     (set-buffer-modified-p nil)
     (goto-char (point-min)))))

(provide 'jmail-view)
