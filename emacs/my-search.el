;;; my-search.el --- Search Configuration

;; Copyright (C) 2020 Julien Masson

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

(require 'rg)
(defun rg-executable () (executable-find "rg"))

;; search with rg
(defun my-search (pattern)
  (interactive "sSearch: ")
  (rg-run pattern "everything" default-directory))

;; search at point
(require 'thingatpt)
(defun my-search-at-point ()
  (interactive)
  (my-search (thing-at-point 'symbol)))

(provide 'my-search)
