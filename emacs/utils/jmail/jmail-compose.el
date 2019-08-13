;;; jmail-compose.el --- XXXX

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

(require 'jmail-company)

;;; Mode

(define-derived-mode jmail-compose-mode message-mode
  "jmail compose"
  (jmail-company-setup))

;;; Internal Functions

(defun jmail-compose--account-list ()
  (when-let ((accounts (jmail-get-accounts jmail-smtp-config-file)))
    (mapcar #'car accounts)))

;;; External Functions

(defun jmail-compose (account)
  (interactive (list (completing-read "Compose with: "
				      (jmail-compose--account-list))))
  (let* ((accounts (jmail-get-accounts jmail-smtp-config-file))
	 (from (assoc-default account accounts))
	 (buffer (message-buffer-name "mail")))
    (with-current-buffer (get-buffer-create buffer)
      (jmail-compose-mode)
      (message-setup `((From . ,(jmail-make-address-str from))
		       (To . "")
		       (Subject . "")))
      (message-sort-headers)
      (message-hide-headers)
      (set-buffer-modified-p nil)
      (message-goto-to)
      (jmail-switch-to-buffer (current-buffer)))))

(provide 'jmail-compose)
