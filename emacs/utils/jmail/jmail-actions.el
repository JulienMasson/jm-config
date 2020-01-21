;;; jmail-actions.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2020-01-14

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

(defcustom jmail-actions '(("patch"        . jmail-apply-patch)
			   ("patch-series" . jmail-apply-patch-series))
  "Alist of actions to apply in `jmail-search-mode'"
  :type 'alist
  :group 'jmail)

;;; Actions

(defun jmail-apply-patch (dir)
  (interactive "DApply patch: ")
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (msg (plist-get object :path))
	       (subject (plist-get object :subject))
	       (default-directory dir)
	       (tmp-patch (concat default-directory ".jmail.patch")))
     (when (string-match "^\\[PATCH " subject)
       (if (not (tramp-tramp-file-p default-directory))
	   (shell-command (concat "git am " msg))
	 (copy-file msg tmp-patch t)
	 (shell-command (concat "git am " (jmail-untramp-path tmp-patch)))
	 (delete-file tmp-patch))))))

(defun jmail-apply-patch-series (dir)
  (interactive "DApply patch series: ")
  (jmail-search--foreach-line-thread
      (when-let* ((object (text-properties-at (point)))
		  (thread (plist-get object :thread))
		  (level (plist-get thread :level))
		  (msg (plist-get object :path))
		  (subject (plist-get object :subject))
		  (default-directory dir)
		  (tmp-patch (concat default-directory ".jmail.patch")))
	(when (and (string-match "^\\[PATCH " subject)
		   (= level 1))
	  (if (not (tramp-tramp-file-p default-directory))
	      (shell-command (concat "git am " msg))
	    (copy-file msg tmp-patch t)
	    (shell-command (concat "git am " (jmail-untramp-path tmp-patch)))
	    (delete-file tmp-patch))))))

;;; External Functions

(defun jmail-actions-apply (action)
  (interactive (list (completing-read "Apply action: "
				      (mapcar #'car jmail-actions))))
  (call-interactively (assoc-default action jmail-actions)))

(provide 'jmail-actions)
