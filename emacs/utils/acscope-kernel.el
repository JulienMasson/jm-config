;;; acscope-kernel.el --- Kernel Database Management

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config.git
;; Created: 2019-05-21

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

(require 'acscope-buffer)
(require 'acscope-database)

;;; Internal vars

(defconst acscope-database--kernel-cross-ref (concat acscope-database--prefix
						      "kernel.out")
  "Kernel cscope cross-reference database file name")

(defconst acscope-database--kernel-source-file (concat acscope-database--prefix
						      "kernel.files")
  "Kernel cscope source file name")

(defconst acscope-database--kernel-fast-symbol `(,(concat acscope-database--prefix
							  "kernel.out.in")
						  ,(concat acscope-database--prefix
							  "kernel.out.po"))
  "Kernel cscope Fast symbol files name")

(defvar acscope-database--kernel-objects-excluded '("vmlinux.o" ".tmp_kallsyms*"))

;;; Internal Functions

(defun acscope-database--kernel-objects-list (dir)
  (let* ((cmd "find . -name \"*.o\"")
	 (str (shell-command-to-string cmd))
	 (objects (split-string str "\n"))
	 (regexp (format "\\(%s\\)"
			 (mapconcat 'identity
				    acscope-database--kernel-objects-excluded
				    "\\|"))))
    (cl-remove-if (lambda (object)
		    (string-match-p regexp object))
		  objects)))

(defun acscope-database--kernel-dirs-from-objects (objects)
  (let (dirs)
    (mapc (lambda (object)
	    (add-to-list 'dirs (file-name-directory object) t))
	  objects)
    dirs))

(defun acscope-database--kernel-headers-list (dirs)
  (let (headers)
    (mapc (lambda (dir)
	    (let* ((cmd (format "find %s -name \"*.h\"" dir))
		   (str (shell-command-to-string cmd))
		   (results (split-string str "\n")))
	      (setq headers (append results headers))))
	  dirs)
    headers))

(defun acscope-database--kernel-default-headers-list ()
  (let* ((cmd "find ./include -name \"*.h\"")
	 (str (shell-command-to-string cmd)))
    (split-string str "\n")))

(defun acscope-database--kernel-source-file-cmd (dir)
  "Create cscope files based on object files found"
  (let* ((default-directory dir)
	 (objects (acscope-database--kernel-objects-list dir))
	 (dirs (acscope-database--kernel-dirs-from-objects objects))
	 (headers (acscope-database--kernel-headers-list dirs))
	 (default-headers (acscope-database--kernel-default-headers-list)))
    (with-current-buffer (find-file-noselect acscope-database--kernel-source-file)
      (erase-buffer)
      (mapc (lambda (object)
	      (insert (replace-regexp-in-string "\.o$" ".c" object))
	      (insert "\n"))
	    objects)
      (mapc (lambda (header)
	      (insert (concat header "\n")))
	    (append headers default-headers))
      (save-buffer))))

(defun acscope-database--kernel-args ()
  "Kernel cscope arguments following database options"
  (append (list "-k" "-i" acscope-database--kernel-source-file
		"-f" acscope-database--kernel-cross-ref)
	  (if acscope-database-fast-symbol '("-q"))))

(defun acscope-database--kernel-check-fast-symbol (dir)
  "Return t if the database respect `acscope-database-fast-symbol',
Otherwise we return nil"
  (let ((fast-symbol-files (mapcar (lambda (file)
				     (concat dir file))
				   acscope-database--kernel-fast-symbol)))
    (if acscope-database-fast-symbol
	(cl-find-if #'file-exists-p fast-symbol-files)
      (cl-find-if-not #'file-exists-p fast-symbol-files))))

(defun acscope-database--kernel-cleanup (dir)
  "Kernel cscope database cleanup"
  (mapc (lambda (file)
	  (let ((default-directory dir))
	    (if (file-exists-p file)
		(delete-file file))))
	(append acscope-database--kernel-fast-symbol
		(list acscope-database--kernel-cross-ref
		      acscope-database--kernel-source-file))))

(defun acscope-database--kernel-check (dir)
  "Kernel cscope database check"
  (let ((default-directory dir))
    (if (file-exists-p acscope-database--kernel-cross-ref)
	(unless (acscope-database--kernel-check-fast-symbol dir)
	  (acscope-database-create "kernel" dir)))))

(defun acscope-database--kernel-create-request (dir)
  "Create kernel cscope database request"
    (let* ((default-directory dir)
	   (args (append (acscope-database--kernel-args) '("-b")))
	   (desc (concat "Creating "
			 (if acscope-database-fast-symbol
			     (propertize "Fast Symbol " 'face 'bold))
			 (propertize "Kernel " 'face 'warning)
			 "cscope database ...\n"))
	   (data (make-acscope-data :dir dir
				    :desc desc))
	   (request (make-acscope-request :program acscope-default-program-name
					  :dir dir :args args
					  :start 'acscope-buffer-init-header
					  :fail 'acscope-buffer-insert-fail
					  :finish 'acscope-database-finish
					  :data data)))
      request))

;;; External Functions

(defun acscope-database-add-kernel (dir)
  "Add kernel database"
  (interactive "DAdd kernel database: ")
  (if (and (file-exists-p (concat dir acscope-database--kernel-cross-ref))
	   (acscope-database--kernel-check-fast-symbol dir))
      (add-to-list 'acscope-database-list dir t)
    (acscope-database-create "kernel" dir)))

(provide 'acscope-kernel)
