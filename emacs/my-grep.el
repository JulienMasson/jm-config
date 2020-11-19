;;; my-grep.el --- Grep Configuration

;; Copyright (C) 2020 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; URL: https://github.com/JulienMasson/jm-config/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; change default grep
(defvar grep-exclude-dirs '(".git" ".ccls-cache"))

(defun grep-get-command (args)
  (let ((default-args "--color -nsrH -E")
	(excludes (mapconcat (lambda (dirs)
			       (format "--exclude-dir=%s" dirs))
			     grep-exclude-dirs " ")))
    (format "grep %s %s %s" excludes default-args args)))

(defun jm-grep (command-args)
  (interactive (list (read-shell-command "Run grep: "
					 "--exclude=cscope* "
					 'grep-history)))
  (grep--save-buffers)
  (compilation-start (grep-get-command command-args) #'grep-mode))
(advice-add 'grep :override #'jm-grep)

;; history
(defvar grep-command-history nil)
(defvar grep-command-index 0)

(defun grep-command-save (command-args)
  (let (data)
    (dolist (command (reverse grep-command-history))
      (unless (and (string= (car command) default-directory)
		   (string= (cadr command) command-args))
	(push command data)))
    (push (list default-directory command-args) data)
    (setq grep-command-history data))
  (setq grep-command-index 0))
(advice-add 'grep :after #'grep-command-save)

(defun grep-command-previous ()
  (interactive)
  (let* ((length (length grep-command-history))
	 (index (if (= grep-command-index (- length 1))
		    0
		  (cl-incf grep-command-index))))
    (setq grep-command-index index)
    (cl-multiple-value-bind (dir command)
	(nth index grep-command-history)
      (setq-local default-directory dir)
      (compilation-start (grep-get-command command) #'grep-mode))))

(defun grep-command-next ()
  (interactive)
  (let* ((length (length grep-command-history))
	 (index (if (zerop grep-command-index)
		    (- length 1)
		  (cl-decf grep-command-index))))
    (setq grep-command-index index)
    (cl-multiple-value-bind (dir command)
	(nth index grep-command-history)
      (setq-local default-directory dir)
      (compilation-start (grep-get-command command) #'grep-mode))))

(defun grep-command-edit (command)
  (interactive (list (let* ((command (nth grep-command-index grep-command-history))
			    (initial-input (cadr command)))
		       (read-string "Run grep: " initial-input))))
  (grep command))

(defun grep-command-change-directory (dir)
  (interactive "DIn directory: ")
  (let ((command (nth grep-command-index grep-command-history)))
    (setq-local default-directory dir)
    (grep (cadr command))))

;; grep context
(require 'grep-context)
(add-hook 'compilation-mode-hook #'grep-context-mode)

(defun my-grep-context (&optional arg)
  (interactive "p")
    (cond ((= arg 1) (call-interactively 'grep-context-more-around-point))
        (t (call-interactively 'grep-context-less-around-point))))

;; grep save buffer
(defun grep-save-buffer ()
  (interactive)
  (rename-buffer (generate-new-buffer-name "*grep*")))

;; wgrep
(require 'wgrep)
(setq wgrep-auto-save-buffer t)

;; grep at point
(require 'thingatpt)
(defun grep-at-point ()
  (interactive)
  (grep (thing-at-point 'symbol)))

(provide 'my-grep)
