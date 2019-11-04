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

;;; Customization

(defcustom jmail-rss-enable nil
  "If non nil, Fetch RSS news"
  :type 'boolean
  :group 'jmail)

(defcustom jmail-rss-config-file nil
  "Config file used by `jmail-rss-program'"
  :type 'string
  :group 'jmail)

;;; External Variables

(defconst jmail-rss-program "feed2exec")

(defvar jmail-rss--current-cb nil)

;;; Internal Functions

(defun jmail-rss--process-sentinel (process status)
  (when-jmail-update-process-success process
      (funcall jmail-rss--current-cb)))

(defun jmail-rss--get-args ()
  (let ((args (list "--verbose" "fetch")))
    (if jmail-rss-config-file
	(append (list "--config" jmail-rss-config-file) args)
      args)))

;;; External Functions

(defun jmail-rss-enabled ()
  (and jmail-rss-enable
       (jmail-find-program jmail-rss-program)))

(defun jmail-rss-fetch (buffer cb)
  (setq jmail-rss--current-cb cb)
  (let* ((default-directory jmail-top-maildir)
	 (program (jmail-find-program jmail-rss-program))
	 (args (jmail-rss--get-args))
	 (process (apply 'start-file-process jmail-update-process-name
			 buffer program args)))
    (set-process-filter process 'jmail-update--process-filter)
    (set-process-sentinel process 'jmail-rss--process-sentinel)))

(provide 'jmail-rss)
