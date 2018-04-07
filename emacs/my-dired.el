;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                DIRED CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ls-lisp)
(require 'dired)

(defun ls-lisp-format-file-size (file-size human-readable)
  "This is a redefinition of the function from `dired.el'. This
fixes the formatting of file sizes in dired mode, to support very
large files. Without this change, dired supports 8 digits max,
which is up to 10gb.  Some files are larger than that.
"
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %11.0f" " %11d") file-size)
    (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
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

;; rebinds `^´ to use the same buffer.
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))))

;; dired async
(require 'dired-async)
(dired-async-mode 1)

;; dired diff
(require 'diff)

(defun get-dired-mark (func)
  (let* ((marks
	  (apply 'append
		 (delq nil
		       (mapcar (lambda (arg)
				 (with-current-buffer (buffer-name arg)
				   (when (string= major-mode "dired-mode")
				     (dired-get-marked-files))))
			       (buffer-list))))))
    (delq nil
	  (mapcar (lambda (arg)
		    (when (funcall func arg)
		      arg))
		  marks))))

(defun apply-all-dired-buffer (func)
  (dolist (buffer (buffer-list))
    (with-current-buffer (buffer-name buffer)
      (when (string= major-mode "dired-mode")
	(funcall func)))))

(defun unmark-all-dired-buffer ()
  (interactive)
  (apply-all-dired-buffer 'dired-unmark-all-marks))

(defun kill-all-dired-buffer ()
  (interactive)
  (apply-all-dired-buffer '(lambda ()
			     (kill-buffer (current-buffer)))))

;; diff for files
(defun dired-diff-files ()
  (interactive)
  (let ((files (get-dired-mark 'file-regular-p)))
    (if (= (length files) 2)
	(diff (car files) (cadr files))
      (error "You should set only two files"))))

;; diff for directories
(defun dired-diff-directories ()
  (interactive)
  (let ((directories (get-dired-mark 'file-directory-p)))
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

;; change keys in dired-mode
(define-key dired-mode-map "=" 'dired-diff-files)
(define-key dired-mode-map "r" 'dired-diff-directories)

;; do hexl-find-file in dired mode
(defun dired-do-hexl-find-file (&optional arg)
  (interactive "P")
  (let ((files (dired-get-marked-files t arg nil nil t)))
    (mapc (lambda (file)
	    (hexl-find-file file))
	  files)))

(define-key dired-mode-map "h" 'dired-do-hexl-find-file)


(provide 'my-dired)
