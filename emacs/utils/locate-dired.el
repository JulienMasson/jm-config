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

(defconst locate-dired--search-header "━▶ Locate search: "
  "Message inserted when searching pattern")

(defvar locate-dired--search--history nil)

;;; Internal Functions

(defun locate-dired--untramp-path (path)
  "Return localname of PATH"
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

(defun locate-dired--switch-to-buffer (buffer)
  "Custom `switch-to-buffer' command"
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer-other-window buffer)))

(defun locate-dired--find-program (program-name)
  "Find PROGRAM-NAME executable in `default-directory'"
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

(defun locate-dired--find-buffer (dir)
  "Return buffer of locate dired buffer"
  (seq-find (lambda (buffer)
	      (with-current-buffer buffer
		(and (string= major-mode "dired-mode")
		     (string= default-directory dir)
		     (string-match-p "^\*locate:" (buffer-name)))))
	    (buffer-list)))

;; (regexp-quote "*locate:")

(defun locate-dired--generate-buffer-name (pattern)
  "Generate locate buffer name"
  (if-let ((buffer (locate-dired--find-buffer default-directory)))
      (buffer-name buffer)
    (generate-new-buffer-name (format "*locate: %s*" pattern))))

(defun locate-dired--create-buffer (name database pattern)
  "Create a locate dired buffer"
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (concat (file-name-directory database) ":\n\n"))
      (insert (concat locate-dired--search-header pattern "\n\n"))
      (dired-mode default-directory locate-dired-switches)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker))))
      (set (make-local-variable 'revert-buffer-function)
	   `(lambda (ignore-auto noconfirm)
	      (locate-dired--create-database)
	      (locate-dired ,pattern)))
      (locate-dired--switch-to-buffer (current-buffer)))))

(defun locate-dired--locate-args (database pattern)
  "Return list of locate arguments"
  (list "--basename" (concat "--database=" database) pattern))

(defun locate-dired--updatedb-args (dir database)
  "Return list of updatedb arguments"
  (list (concat "--localpaths=" dir)
	(concat "--output=" database)
	(if locate-dired-prunepaths
	    (format "--prunepaths=\"%s\""
		    (mapconcat (lambda (elem)
				 (concat dir elem))
			       locate-dired-prunepaths " ")))))

(defun locate-dired--move-to-search ()
  "Move point to search results"
  (goto-char (point-min))
  (search-forward locate-dired--search-header nil t)
  (forward-line)
  (skip-chars-forward " \t\n"))

(defun locate-dired--get-files ()
  "Parse each line from point and return a list of files"
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
  "Process results when the process has finished"
  (with-current-buffer (process-buffer process)
    (locate-dired--move-to-search)
    (let ((files (locate-dired--get-files))
	  (inhibit-read-only t))
      (delete-region (point) (point-max))
      (if files
	  (apply 'process-file "ls" nil (current-buffer) nil
		 locate-dired-switches files)
	(insert "--- No files found ---\n"))
      (insert (concat "\nLocate finished at " (current-time-string)))
      (indent-rigidly (point-min) (point-max) 2))))

(defun locate-dired--process-filter (process str)
  "Insert result in the PROCESS buffer"
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str))))

(defun locate-dired--search (database pattern)
  "Search PATTERN in DATABASE"
  (if-let ((locate (locate-dired--find-program locate-dired--locate)))
      (let* ((buffer-name (locate-dired--generate-buffer-name pattern))
	     (buffer (locate-dired--create-buffer buffer-name database pattern))
	     (local-database (locate-dired--untramp-path database))
	     (args (locate-dired--locate-args local-database pattern))
	     (process (apply 'start-file-process "locate" buffer
			     locate args)))
	(set-process-filter process 'locate-dired--process-filter)
	(set-process-sentinel process 'locate-dired--process-sentinel))
    (message (concat locate-dired--locate " not found !"))))

(defun locate-dired--create-database ()
  "Create locate database in `default-directory' and return the database path."
  (if-let ((updatedb (locate-dired--find-program locate-dired--updatedb)))
      (let* ((dir (expand-file-name default-directory))
	     (local-dir (locate-dired--untramp-path dir))
	     (database (concat local-dir locate-dired--database))
	     (args (locate-dired--updatedb-args local-dir database)))
	(message "Creating locate database ...")
	(apply 'process-file updatedb nil nil nil args)
	(message nil)
	(concat dir locate-dired--database))
    (message (concat locate-dired--updatedb " not found !"))
    nil))

(defun locate-dired--find-database ()
  "Return database found in `default-directory' or create new one."
  (let* ((database (expand-file-name
		    (concat default-directory locate-dired--database)))
	 (prompt (format "Create locate database (%s): "
			 (propertize database 'face 'success))))
    (if (file-exists-p database)
	database
      (when (yes-or-no-p prompt)
	(locate-dired--create-database)))))

;;; External Functions

(defun locate-dired (pattern)
  "Search PATTERN from current `default-directory'.
If no database is found, we ask to create one."
  (interactive (list (read-string "Locate search: " nil
				  'locate-dired--search--history)))
  (when-let ((database (locate-dired--find-database)))
    (locate-dired--search database pattern)))

(provide 'locate-dired)
