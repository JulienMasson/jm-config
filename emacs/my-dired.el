;;; my-dired.el --- Dired Configuration

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

(require 'ls-lisp)
(require 'dired)
(require 'dired-x)

;; switches passed to ls
(setq dired-listing-switches "-alh")

;; delete recursively without asking
(setq dired-recursive-deletes 'always)

;; default dired setting
(put 'dired-find-alternate-file 'disabled nil)

;; open pdf with evince
(assoc-delete-all "\\.pdf\\'" dired-guess-shell-alist-default)
(add-to-list 'dired-guess-shell-alist-default (list "\\.pdf\\'" "evince"))

;; locate dired
(require 'locate-dired)

;; async dired
(require 'async-dired)
(async-dired-setup)

;; sort dired buffer
(defun ls-lisp-format-file-size (file-size human-readable)
  "This is a redefinition of the function from `dired.el'. This
fixes the formatting of file sizes in dired mode, to support very
large files. Without this change, dired supports 8 digits max,
which is up to 10gb.  Some files are larger than that.
"
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %11.0f" " %11d") file-size)
    (cl-do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
            ;; kilo, mega, giga, tera, peta, exa
            (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
        ((< file-size 1024) (format " %10.0f%s"  file-size (car post-fixes))))))


(defun dired-sort-toggle ()
  "This is a redefinition of the fn from dired.el. Normally,
dired sorts on either name or time, and you can swap between them
with the s key.  This function one sets sorting on name, size,
time, and extension. Cycling works the same.
"
  (setq dired-actual-switches
        (let (case-fold-search)
          (cond
           ((string-match " " dired-actual-switches) ;; contains a space
            ;; New toggle scheme: add/remove a trailing " -t" " -S",
            ;; or " -U"
            ;; -t = sort by time (date)
            ;; -S = sort by size
            ;; -X = sort by extension

            (cond

             ((string-match " -t\\'" dired-actual-switches)
              (concat
               (substring dired-actual-switches 0 (match-beginning 0))
               " -X"))

             ((string-match " -X\\'" dired-actual-switches)
              (concat
               (substring dired-actual-switches 0 (match-beginning 0))
               " -S"))

             ((string-match " -S\\'" dired-actual-switches)
              (substring dired-actual-switches 0 (match-beginning 0)))

             (t
              (concat dired-actual-switches " -t"))))

           (t
            ;; old toggle scheme: look for a sorting switch, one of [tUXS]
            ;; and switch between them. Assume there is only ONE present.
            (let* ((old-sorting-switch
                    (if (string-match (concat "[t" dired-ls-sorting-switches "]")
                                      dired-actual-switches)
                        (substring dired-actual-switches (match-beginning 0)
                                   (match-end 0))
                      ""))

                   (new-sorting-switch
                    (cond
                     ((string= old-sorting-switch "t") "X")
                     ((string= old-sorting-switch "X") "S")
                     ((string= old-sorting-switch "S") "")
                     (t "t"))))
              (concat
               "-l"
               ;; strip -l and any sorting switches
               (dired-replace-in-string (concat "[-lt"
                                                dired-ls-sorting-switches "]")
                                        ""
                                        dired-actual-switches)
               new-sorting-switch))))))

  (dired-sort-set-modeline)
  (revert-buffer))


(defun dired-sort-set-modeline ()
  "This is a redefinition of the fn from `dired.el'. This one
properly provides the modeline in dired mode, supporting the new
search modes defined in the new `dired-sort-toggle'.
"
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
                   "Dired by time")
                  ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
                   "Dired by extension")
                  ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
                   "Dired by size")
                  ((string-match "^-[^SXUt]*$" dired-actual-switches)
                   "Dired by name")
                  (t
                   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))

;; dired diff
(require 'diff)

(defun apply-all-dired-buffer (func)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (string= major-mode "dired-mode")
	(funcall func)))))

(defun my-dired-get-marked ()
  (save-excursion
    (let (results)
      (goto-char (point-min))
      (while (re-search-forward (dired-marker-regexp) nil t)
	(push (dired-get-filename) results)
	(forward-line 1))
      results)))

(defun dired-get-all-marked ()
  (let (results marks)
    (mapc (lambda (buffer)
	    (with-current-buffer buffer
	      (when (string= major-mode "dired-mode")
		(setq marks (my-dired-get-marked))
		(when marks
		  (push marks results)))))
	  (buffer-list))
    (apply 'append results)))

(defun unmark-all-dired-buffer ()
  (interactive)
  (apply-all-dired-buffer #'dired-unmark-all-marks))

(defun kill-all-dired-buffer ()
  (interactive)
  (apply-all-dired-buffer #'kill-current-buffer))

;; diff for files
(defun dired-diff-files ()
  (interactive)
  (let ((files (seq-filter #'file-regular-p (dired-get-all-marked))))
    (if (= (length files) 2)
	(diff (car files) (cadr files))
      (error "You should set only two files"))))

;; diff for directories
(defun dired-diff-directories ()
  (interactive)
  (let ((directories (seq-filter #'file-directory-p
				 (dired-get-all-marked))))
    (if (= (length directories) 2)
	(let ((directory-1 (car directories))
	      (directory-2 (cadr directories)))
	  (if (or (file-remote-p directory-1)
		  (file-remote-p directory-2))
	      (error "Cannot diff with remote directories")
	    (let* ((buf (get-buffer-create "*Diff*"))
		   (command (format "diff -r %s %s"
				    directory-1 directory-2))
		   (thisdir default-directory))
	      (with-current-buffer buf
		(setq buffer-read-only t)
		(buffer-disable-undo (current-buffer))
		(let ((inhibit-read-only t))
		  (erase-buffer))
		(buffer-enable-undo (current-buffer))
		(diff-mode)
		(setq default-directory thisdir)
		(let ((inhibit-read-only t))
		  (insert command "\n"))
		(let ((proc (start-process "Diff" buf shell-file-name
					   shell-command-switch command)))
		  (set-process-filter proc 'diff-process-filter)
		  (set-process-sentinel
		   proc (lambda (proc _msg)
			  (with-current-buffer (process-buffer proc)
			    (diff-sentinel (process-exit-status proc))))))
		(switch-to-buffer buf)))))
      (error "You should set only two directories"))))

;; do hexl-find-file in dired mode
(defun dired-do-hexl-find-file (&optional arg)
  (interactive "P")
  (let ((files (dired-get-marked-files t arg)))
    (mapc (lambda (file)
	    (hexl-find-file file))
	  files)))

;; dired sudo
(defun dired-sudo ()
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (tramp-tramp-file-p dir)
	(let* ((dissect (tramp-dissect-file-name dir))
	       (host (tramp-file-name-host dissect))
	       (local-dir (tramp-file-name-localname dissect))
	       (tramp (replace-regexp-in-string local-dir "" dir)))
	  (dired (format "%s|sudo:root@%s:%s" tramp host local-dir)))
      (dired (concat "/sudo:root@localhost:" dir)))))

;; fd find-name
(defvar fd-find-buffer "*fd-find*")
(defvar fd-find-history '())
(defvar-local fd-find--start nil)

(defun fd-find-dired--process-filter (process str)
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
	  (save-excursion
	    (goto-char (point-max))
	    (dolist (line (split-string str "\n"))
	      (insert (format "  %s\n" line)))))))))

(defun fd-find-dired--process-sentinel (process status)
  (when (eq (process-exit-status process) 0)
    (with-current-buffer (process-buffer process)
      (let* ((inhibit-read-only t)
	     (time-elapsed (time-since fd-find--start))
	     (time-str (format-time-string "%s.%3N" time-elapsed)))
	(save-excursion
	  (goto-char (point-max))
	  (insert (format "  Search took: %s seconds" time-str)))))))

(defun fd-find-setup-buffer (dir pattern cmd)
  (with-current-buffer (get-buffer-create fd-find-buffer)
    (widen)
    (kill-all-local-variables)
    (setq default-directory dir)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "  %s:\n\n" default-directory))
      (insert (format "  %s\n\n" cmd)))
    (dired-mode default-directory "-dilsb")
    (setq fd-find--start (current-time))
    (setq dired-buffers (cl-remove (current-buffer) dired-buffers
				   :key #'cdr :test #'equal))
    (setq-local revert-buffer-function `(lambda (ignore-auto noconfirm)
					  (fd-find-dired ,pattern)))
    (setq-local dired-subdir-alist (list (cons default-directory (point-min-marker))))
    (setq mode-line-process '(":%s"))
    (current-buffer)))

(defun fd-find-dired (pattern)
  ;; check if a process is running
  (when-let ((process (get-buffer-process fd-find-buffer)))
    (when (eq (process-status process) 'run)
      (if (yes-or-no-p "Stop current search ?")
	  (progn
	    (interrupt-process process)
	    (sit-for 1)
	    (delete-process process))
	(error "Cannot run two searchs in same time"))))

  ;; start process
  (let* ((fd-cmd (format "fd -I -0 -c never \"%s\"" pattern))
	 (fd-ls "xargs -0 ls -ld --quoting-style=literal")
	 (cmd (format "%s | %s" fd-cmd fd-ls))
	 (buffer (fd-find-setup-buffer default-directory pattern cmd))
	 (process (start-file-process "fd" buffer "bash" "-c" cmd)))
    (set-process-filter process 'fd-find-dired--process-filter)
    (set-process-sentinel process 'fd-find-dired--process-sentinel)
    (if (get-buffer-window-list buffer)
	(pop-to-buffer buffer)
      (pop-to-buffer-same-window buffer))))

(defun fd-find-name-dired ()
  (interactive)
  (if (executable-find "fd" t)
      (let* ((header (format "[%s] " (propertize "fd" 'face 'success)))
	     (pattern (read-string (concat header "Find file: ")
				   nil 'fd-find-history)))
	(fd-find-dired pattern))
    (call-interactively #'find-name-dired)))

(provide 'my-dired)
