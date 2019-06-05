;;; locate-dired.el --- Locate in Dired Mode

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config.git
;; Created: 2019-06-03

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

(require 'dired)
(require 'subr-x)
(require 'tramp)

;;; External vars

(defcustom locate-dired-switches "-dilsb"
  "Switches arguments used when performing ls on files found")

(defcustom locate-dired-prunepaths (list ".bzr" ".hg" ".git" ".svn" ".repo")
  "List of directories to not put in the locate database")

;;; Internal vars

(defconst locate-dired--updatedb "updatedb.findutils"
  "Executable to update/create a locate database")

(defconst locate-dired--locate "locate.findutils"
  "Executable to list files in databases that match a pattern")

(defconst locate-dired--database "locate.db"
  "Locate database file name")

(defconst locate-dired--search-header "  ━▶ Locate search: "
  "Message inserted when searching pattern")

(defconst locate-dired--create-header "  ━▶ Creating locate database ..."
  "Message inserted when creating locate database")

(defvar locate-dired--search--history nil)

;;; Internal Functions

(defun locate-dired--untramp-path (path)
  "Return localname of PATH."
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

(defun locate-dired--insert (buffer str)
  "Insert SRT in locate dired BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str))))

(defun locate-dired--switch-to-buffer (buffer)
  "Custom `switch-to-buffer' command."
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)))

(defun locate-dired--find-program (program-name)
  "Find PROGRAM-NAME executable in `default-directory'."
  (if (tramp-tramp-file-p default-directory)
      (with-parsed-tramp-file-name default-directory nil
	(let ((buffer (tramp-get-connection-buffer v))
	      (cmd (concat "which " program-name)))
	  (with-current-buffer buffer
	    (tramp-send-command v cmd)
	    (goto-char (point-min))
	    (when (looking-at "^\\(.+\\)")
	      (match-string 1)))))
    (executable-find program-name)))

(defun locate-dired--find-buffer (database pattern)
  "Return buffer of locate dired buffer."
  (seq-find (lambda (buffer)
	      (with-current-buffer buffer
		(let ((dtb (get-text-property (point-min) 'locate-database))
		      (pat (get-text-property (point-min) 'locate-pattern)))
		  (and (string= major-mode "dired-mode")
		       (string= database dtb)
		       (string= pattern pat)))))
	    (buffer-list)))

(defun locate-dired--buffer-name (database pattern)
  "Generate locate buffer name."
  (if-let ((buffer (locate-dired--find-buffer database pattern)))
      (buffer-name buffer)
    (generate-new-buffer-name (format "*locate: %s*" pattern))))

(defun locate-dired--create-buffer (database pattern)
  "Create a locate dired buffer."
  (let ((plist `(locate-database ,database locate-pattern ,pattern))
	(buffer-name (locate-dired--buffer-name database pattern))
	(inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "  %s:\n\n" (file-name-directory database)))
      (add-text-properties (point-min) (point-max) plist)
      (dired-mode default-directory locate-dired-switches)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker))))
      (set (make-local-variable 'revert-buffer-function)
	   `(lambda (ignore-auto noconfirm)
	      (locate-dired--create-search ,database ,pattern)))
      (locate-dired--switch-to-buffer (current-buffer)))))

(defun locate-dired--locate-args (database pattern)
  "Return list of locate arguments."
  (list "--basename" (concat "--database=" database) pattern))

(defun locate-dired--updatedb-args (database)
  "Return list of updatedb arguments."
  (let* ((dir (file-name-directory database))
	 (prunepaths (if locate-dired-prunepaths
			 (mapconcat (lambda (elem)
				      (concat dir elem))
				    locate-dired-prunepaths " "))))
    (list (concat "--localpaths=" dir)
	  (if prunepaths
	      (format "--prunepaths=\"%s\"" prunepaths))
	  (concat "--output=" database))))

(defun locate-dired--move-to-search ()
  "Move point to search results."
  (goto-char (point-min))
  (search-forward locate-dired--search-header nil t)
  (forward-line)
  (skip-chars-forward " \t\n"))

(defun locate-dired--get-files ()
  "Parse each line from point and return a list of files."
  (let (files)
    (save-excursion
      (while (null (eobp))
	(let* ((dir (expand-file-name default-directory))
	       (beg (line-beginning-position))
	       (end (line-end-position))
	       (line (buffer-substring-no-properties beg end))
	       (local-dir (locate-dired--untramp-path dir))
	       (file (replace-regexp-in-string local-dir "" line)))
	  (add-to-list 'files file t))
	(forward-line)))
    files))

(defun locate-dired--process-sentinel (process status)
  "Process results when the process has finished."
  (with-current-buffer (process-buffer process)
    (locate-dired--move-to-search)
    (let ((files (locate-dired--get-files))
	  (beg (point))
	  (inhibit-read-only t))
      (delete-region beg (point-max))
      (if files
      	  (apply 'process-file "ls" nil (current-buffer) nil
      		 locate-dired-switches files)
      	(insert "--- No files found ---\n"))
      (insert (concat "\nLocate finished at " (current-time-string)))
      (indent-rigidly beg (point-max) 2))))

(defun locate-dired--process-filter (process str)
  "Insert result in the PROCESS buffer."
  (locate-dired--insert (process-buffer process) str))

(defun locate-dired--search (database pattern)
  "Search PATTERN in DATABASE."
  (let ((buffer (locate-dired--create-buffer database pattern)))
    (if-let ((locate (locate-dired--find-program locate-dired--locate)))
	(let* ((local-database (locate-dired--untramp-path
				(expand-file-name database)))
	       (args (locate-dired--locate-args local-database pattern))
	       (process (apply 'start-file-process "locate" buffer
			       locate args)))
	  (locate-dired--insert buffer (concat locate-dired--search-header
					       pattern "\n\n"))
	  (set-process-filter process 'locate-dired--process-filter)
	  (set-process-sentinel process 'locate-dired--process-sentinel))
      (locate-dired--insert buffer (concat locate-dired--locate " not found !")))))

(defun locate-dired--process-create-sentinel (process status)
  "Remove text inserted when creating database and search pattern."
  (with-current-buffer (process-buffer process)
    (let ((database (get-text-property (point-min) 'locate-database))
	  (pattern (get-text-property (point-min) 'locate-pattern))
	  (inhibit-read-only t))
      (goto-char (point-min))
      (search-forward locate-dired--create-header nil t)
      (delete-region (point) (point-max))
      (locate-dired--search database pattern))))

(defun locate-dired--create-search (database pattern)
  "Create locate DATABASE and search PATTERN."
  (let ((buffer (locate-dired--create-buffer database pattern)))
    (if-let ((updatedb (locate-dired--find-program locate-dired--updatedb)))
	(let* ((local-database (locate-dired--untramp-path
				(expand-file-name database)))
	       (args (locate-dired--updatedb-args local-database))
	       (process (apply 'start-file-process "updatedb" buffer
			       updatedb args)))
	  (locate-dired--insert buffer locate-dired--create-header)
	  (set-process-sentinel process 'locate-dired--process-create-sentinel))
      (locate-dired--insert buffer (concat locate-dired--updatedb " not found !")))))

;;; External Functions

(defun locate-dired (pattern)
  "Search PATTERN from current `default-directory'.
If no database is found, we ask to create one and process the request."
  (interactive (list (read-string "Locate search: " nil
				  'locate-dired--search--history)))
  (let* ((database (concat default-directory locate-dired--database))
	 (prompt (format "Create locate database (%s): "
			 (propertize database 'face 'success))))
    (if (file-exists-p database)
	(locate-dired--search database pattern)
      (when (yes-or-no-p prompt)
	(locate-dired--create-search database pattern)))))

(provide 'locate-dired)
