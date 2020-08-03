;;; jmail-rss.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-10-29

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

(require 'json)

;;; Customization

(defcustom jmail-rss-enable nil
  "If non nil, Fetch RSS news"
  :type 'boolean
  :group 'jmail)

(defcustom jmail-rss-config-file nil
  "Config file used by `jmail-rss-program'"
  :type 'string
  :group 'jmail)

(defcustom jmail-rss-fetch-after (* 30 60)
  "Fetch RSS news after X seconds, the exact time depends on the time to fetch mails"
  :type 'integer
  :group 'jmail)

;;; External Variables

(defconst jmail-rss-program "feed2exec")

(defvar jmail-rss--current-cb nil)

(defvar jmail-rss--current-time 0)

;;; Internal Functions

(defun jmail-rss--get-args (cmd)
  (let ((args (list "--verbose" cmd)))
    (if jmail-rss-config-file
	(append (list "--config" jmail-rss-config-file) args)
      args)))

(defun jmail-rss--get-folders ()
  (let ((args (jmail-rss--get-args "ls"))
	folders)
    (with-temp-buffer
      (apply 'process-file jmail-rss-program nil
	     (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
	(when-let ((object (ignore-errors (json-read))))
	  (push object folders))
	(end-of-defun)))
    (mapcar (lambda (elem)
	      (let ((mailbox (file-name-as-directory
			      (assoc-default 'mailbox elem)))
		    (folder (assoc-default 'folder elem)))
	      (file-name-as-directory (concat mailbox folder))))
	  folders)))

(defun jmail-rss--get-count (dir)
  (let* ((hostname (getenv "HOSTNAME"))
	 (regexp (format ".*%s,U=\\([0-9]+\\):2," hostname))
	 (last-file (car (last (directory-files dir)))))
    (if (string-match regexp last-file)
	(string-to-number (match-string 1 last-file))
      0)))

(defun jmail-rss--rename-file (file count)
  (let* ((hostname (getenv "HOSTNAME"))
	 (regexp (concat hostname "$")))
    (when (string-match regexp file)
      (rename-file file (format "%s,U=%d:2," file count)))))

(defun jmail-rss--rename-new-entries ()
  (let ((folders (jmail-rss--get-folders)))
    (mapc (lambda (folder)
	    (let* ((cur-dir (concat folder "cur/"))
		   (new-dir (concat folder "new/"))
		   (count (max (jmail-rss--get-count cur-dir)
			       (jmail-rss--get-count new-dir)))
		   (new-files (directory-files new-dir t "^[^.]")))
	      (mapc (lambda (file)
		      (setq count (+ count 1))
		      (jmail-rss--rename-file file count))
		    new-files)))
	  folders)))

(defun jmail-rss--process-sentinel (process status)
  (when-jmail-update-process-success process
      (jmail-rss--rename-new-entries)
      (setq jmail-rss--current-time (time-convert (current-time) 'integer))
      (funcall jmail-rss--current-cb)))

;;; External Functions

(defun jmail-rss-enabled ()
  (and jmail-rss-enable
       (jmail-find-program jmail-rss-program)))

(defun jmail-rss-fetch (buffer cb)
  (let ((secs (time-convert (current-time) 'integer)))
    (if (< secs (+ jmail-rss--current-time jmail-rss-fetch-after))
	(funcall cb)
      (setq jmail-rss--current-cb cb)
      (let* ((default-directory jmail-top-maildir)
	     (program (jmail-find-program jmail-rss-program))
	     (args (jmail-rss--get-args "fetch"))
	     (process (apply 'start-file-process jmail-update-process-name
			     buffer program args)))
	(set-process-filter process 'jmail-update--process-filter)
	(set-process-sentinel process 'jmail-rss--process-sentinel)))))

(provide 'jmail-rss)
