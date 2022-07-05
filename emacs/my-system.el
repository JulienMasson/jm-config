;;; my-system.el --- System Configuration

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

;; emacs specific files
(setq user-emacs-directory "~/.config/.emacs.d/")

;; silent warning
(setq warning-suppress-types '((comp)))

;; shifted motion keys activate the mark momentarily
(setq shift-select-mode t)

;; prevent automatic splitting
(set-frame-parameter nil 'unsplittable t)

;; movement off the edge of the frame wraps around.
(setq windmove-wrap-around t)

;; enable upcase/dowcase commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable narrowing commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; disable use of directory-local variables
(setq enable-dir-local-variables nil)

;; don't ask to follow symlink
(setq vc-follow-symlinks t)

;; remove eldoc mode
(global-eldoc-mode -1)

;; disable erase-buffer
(put 'erase-buffer 'disabled nil)

;; save password
(setq password-cache-expiry nil)

;; performance mitigations for files with long lines
(global-so-long-mode)

;; never request confirmation when opening large file
(setq large-file-warning-threshold nil)

;; save backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; delete duplicates in history
(setq history-delete-duplicates t)

;; yes-or-no
(defun completing-read-yes-or-no-p (prompt)
  (let ((answer (completing-read prompt '("YES" "NO") nil t)))
    (string= answer "YES")))
(advice-add 'yes-or-no-p :override #'completing-read-yes-or-no-p)

;; silent push-mark
(defun silent-push-mark (func &rest args)
  (pcase-let ((`(,location ,_ ,activate) args))
    (funcall func location t activate)))
(advice-add 'push-mark :around #'silent-push-mark)

(provide 'my-system)
