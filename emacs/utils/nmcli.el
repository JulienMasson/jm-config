;;; nmcli.el

;; Copyright (C) 2017 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>

;; This file is NOT part of GNU Emacs.

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


(defun connect-wifi (target)
  (interactive (list (ido-completing-read "Connect: "
					  (split-string
					   (shell-command-to-string
					    "nmcli -g=SSID device wifi list")
					   "\n")
					  nil t nil nil)))
  (let ((password (read-from-minibuffer "Password: ")))
    (shell-command (format "nmcli device wifi connect %s password %s"
			   target
			   password))))


(provide 'nmcli)
