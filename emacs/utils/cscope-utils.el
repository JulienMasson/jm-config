;;; cscope-utils.el --- Some Cscope Utils functions

;; Copyright (C) 2019 Julien Masson

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

(require 'cscope)

;; dired in cscope-database-list
(defun cscope-build-assoc-database ()
  (mapcar (lambda (path)
	    (cons (file-name-base (directory-file-name path))
		  path))
	  cscope-database-list))

(defun cscope-dired-directory ()
  (interactive)
  (if-let* ((databases (cscope-build-assoc-database))
	    (target (completing-read "Dired to: "
				     (mapcar #'car databases)))
	    (path (assoc-default target databases)))
      (dired path)))

;; generate cscope files from objects files path
(defvar cscope-objects-excluded '("vmlinux.o" ".tmp_kallsyms*"))

(defun cscope-objects-list (dir)
  (let* ((cmd "find . -name \"*.o\"")
	 (str (shell-command-to-string cmd))
	 (objects (split-string str "\n"))
	 (regexp (format "\\(%s\\)"
			 (mapconcat 'identity cscope-objects-excluded
				    "\\|"))))
    (cl-remove-if (lambda (object)
		    (string-match-p regexp object))
		  objects)))

(defun cscope-get-dirs-from-objects (objects)
  (let (dirs)
    (mapc (lambda (object)
	    (add-to-list 'dirs (file-name-directory object) t))
	  objects)
    dirs))

(defun cscope-headers-list (dirs)
  (let (headers)
    (mapc (lambda (dir)
	    (let* ((cmd (format "find %s -name \"*.h\"" dir))
		   (str (shell-command-to-string cmd))
		   (results (split-string str "\n")))
	      (setq headers (append results headers))))
	  dirs)
    headers))

(defun cscope-default-headers-list ()
  (let* ((cmd "find ./include -name \"*.h\"")
	 (str (shell-command-to-string cmd)))
    (split-string str "\n")))

(defun cscope-generate-from-objects (dir)
  (interactive "DAdd database from objects: ")
  (let* ((objects (cscope-objects-list dir))
	 (dirs (cscope-get-dirs-from-objects objects))
	 (headers (cscope-headers-list dirs))
	 (default-headers (cscope-default-headers-list)))
    (with-current-buffer (find-file-noselect cscope-index-file)
      (erase-buffer)
      (mapc (lambda (object)
	      (insert (replace-regexp-in-string "\.o$" ".c" object))
	      (insert "\n"))
	    objects)
      (mapc (lambda (header)
	      (insert (concat header "\n")))
	    (append headers default-headers))
      (save-buffer))))

;; add database with pycscope
(defun cscope-create-database-pycscope (dir)
  (cscope-check-env)
  (let* ((default-directory dir)
	 (cscope-program-name "pycscope")
	 (find-cmd (concat "find . -name \"*.py\" > " cscope-index-file))
	 (cmd '("-D" "-R"))
	 (desc "Creating database cscope with pycscope ...\n")
	 (data (make-cscope-data :dir dir
				 :desc desc))
	 (request (make-cscope-request :dir dir :cmd cmd
				       :start 'cscope-insert-initial-header
				       :fail 'cscope-insert-request-fail
				       :finish 'cscope-database-finish
				       :data data)))
    (unless (file-exists-p cscope-index-file)
      (shell-command find-cmd))
    (cscope-process-request request)))

(defun cscope-add-database-pycscope (dir)
  (interactive "DAdd database pycscope: ")
    (if (file-exists-p (concat dir cscope-database-file))
	(add-to-list 'cscope-database-list dir t)
      (cscope-create-database-pycscope dir)))

(provide 'cscope-utils)
