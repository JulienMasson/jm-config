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

(defvar acscope-database--kernel-default-header-dirs '("./include/"))

;;; Internal Functions

(defun acscope-database--kernel-filter-dirs (dirs)
  (let ((dirs-sorted (seq-sort 'string-lessp dirs)))
    (seq-uniq dirs-sorted (lambda (a b)
			    (string-match-p (regexp-quote b) a)))))

(defun acscope-database--kernel-keep-regexp (dirs)
  (let* ((all-dirs (append dirs acscope-database--kernel-default-header-dirs))
	 (all (acscope-database--kernel-filter-dirs all-dirs)))
    (format "^\\(%s\\).*$" (mapconcat 'regexp-quote all "\\|"))))

(defun acscope-database--kernel-dir-name ()
  (let* ((beg (line-beginning-position))
    	 (end (line-end-position))
    	 (str (buffer-substring beg end)))
    (file-name-directory str)))

(defun acscope-database--kernel-source-file-cmd (dir)
  "Create cscope files based on object files found"
  (let ((default-directory dir)
	(save-silently t)
	object-dirs beg)
    (with-current-buffer (find-file-noselect acscope-database--kernel-source-file t)
      (erase-buffer)
      (process-file "find" nil (current-buffer) nil "-name" "*.o" "-mindepth" "2" )
      (goto-char (point-min))
      (while (re-search-forward "\.o$" nil t)
      	(add-to-list 'object-dirs (acscope-database--kernel-dir-name))
      	(replace-match ".c"))
      (insert "\n")
      (setq beg (point))
      (process-file "find" nil (current-buffer) nil "-name" "*.h" "-mindepth" "2" )
      (keep-lines (acscope-database--kernel-keep-regexp object-dirs)
      		  beg (point-max))
      (save-buffer)
      (kill-buffer))
    (message nil)))

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
