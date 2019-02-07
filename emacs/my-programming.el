;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump only on error compilation
(setq compilation-skip-threshold 2)

;; ansi color on compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; gdb config
(require 'load-relative)
(require 'loc-changes)
(require 'realgud)
(setq realgud-safe-mode nil)

(defun gdb (file)
  (interactive (list (ido-read-file-name "gdb on: ")))
  (realgud:gdb (concat "gdb " file)))

(defun gdb-attach (process)
  (interactive "sProcess Name: ")
  (let* ((user (shell-command-to-string "echo -n $USER"))
	 (regexp (format "^%s\s*(\\d+).*%s$"
			 user process))
	 (cmd (format "ps aux | perl -ne 'print \"$1\" if /%s/'"
		      regexp))
	 (pid (shell-command-to-string cmd)))
    (when pid
      (realgud:gdb-pid (string-to-number pid)))))

(defun my-realgud-file-find-function (marker filename directory &rest formats)
  (concat (f-root) filename))

(setq realgud-file-find-function 'my-realgud-file-find-function)

;; visual regexp
(require 'visual-regexp)

;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; default indentation
(setq c-default-style '((c-mode . "linux") (other . "gnu")))

;; save backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; cscope
(require 'xcscope)
(setq cscope-option-use-inverted-index t)
(setq cscope-option-kernel-mode t)
(setq cscope-display-buffer-args nil)
(cscope-setup)

(defun cscope-regenerate-cscope-files (top-directory)
  (shell-command "find . -name \"*.[chxsS]\" > cscope.files"))
(advice-add 'cscope-index-files :after #'cscope-regenerate-cscope-files)

;; cscope search list
(defvar jm-cscope-search-list nil)
(defun cscope-add-cscope-search-list (dir)
  "Add cscope database to search list."
  (interactive "DAdd database: ")
  (setq jm-cscope-search-list (add-to-list 'jm-cscope-search-list (list dir))))

(defun cscope-reset-cscope-search-list ()
  "Rest cscope search list"
  (interactive)
  (setq jm-cscope-search-list nil))

;; auto-detection indenting
(require 'dtrt-indent)
(dtrt-indent-mode t)

;; change default grep
(setq grep-command "grep --color -nsrH -E ")

;; grep context
(require 'grep-context)
(add-hook 'compilation-mode-hook #'grep-context-mode)

(defun my-grep-context (&optional arg)
  (interactive "p")
    (cond ((= arg 1) (call-interactively 'grep-context-more-around-point))
        (t (call-interactively 'grep-context-less-around-point))))

;; grep save buffer
(defun grep-save-buffer ()
  (interactive)
  (rename-buffer (generate-new-buffer-name "*grep*")))

;; grep at point
(require 'thingatpt)
(defun grep-at-point ()
  (interactive)
  (grep (format "%s%s ."
		grep-command
		(thing-at-point 'symbol))))

;; occur at point
(defun occur-at-point ()
  (interactive)
  (occur (thing-at-point 'symbol)))

;; ripgrep
(require 'rg)

;; company mode
(require 'company)
(setq company-backends nil)
(add-hook 'after-init-hook 'global-company-mode)

;; completion at point company
(require 'company-capf)
(add-to-list 'company-backends 'company-capf)

;; ;; files company
(require 'company-files)
(add-to-list 'company-backends 'company-files)

;; clang company
(require 'company-clang)
(add-to-list 'company-backends 'company-clang)

(defvar cscope-include-dirs-cached nil)

(defun cscope-generate-include-dirs (dir)
  (unless (assoc dir cscope-include-dirs-cached)
    (let* ((default-directory dir)
	   (search-regex "\\.\\/(.*)\\/.*h$")
	   (search-cmd (concat
			"cat cscope.files |"
			"perl -ne 'print \"$1\n\" if /"
			search-regex
			"/' |"
			"sort | uniq"))
	   (directories (split-string (shell-command-to-string
				       search-cmd))))
      (add-to-list 'cscope-include-dirs-cached
		   `(,dir . ,(mapcar
			      (lambda (arg)
				(format "-I%s" (untramp-path
						(concat dir arg))))
			      directories)))))
  (assoc-default dir cscope-include-dirs-cached))

(defun extra-args-clang-company ()
  (when jm-cscope-search-list
      (setq-local company-clang-arguments
		  (apply 'append
			 (mapcar (lambda (arg)
				   (cscope-generate-include-dirs (car arg)))
				 jm-cscope-search-list)))))

(add-hook 'c-mode-hook 'extra-args-clang-company)

;; c header company
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;; perl
(defalias 'perl-mode 'cperl-mode)

;; ctags
(defun create-ctags-tag (dir)
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (shell-command "/usr/bin/ctags -e -R .")))

;; anaconda company
(require 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)

;; pycscope
(defun cscope-pycscope (dir)
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (async-shell-command "pycscope -D -R . && find . -name '*.py' > cscope.files")))

(add-hook 'python-mode-hook (function cscope-minor-mode))

;; manual at point
(defun manual-at-point()
  (interactive)
  (cond
   ;; C mode
   ((string= major-mode "c-mode")
    (let* ((pattern (thing-at-point 'symbol))
	   (man-result (split-string
			(shell-command-to-string (format "man -f %s" pattern)) "\n"))
	   (match (car (delq nil
			     (mapcar (lambda (line)
				       (if (string-match (format "\\(%s ([23])\\).*" pattern) line)
					   (match-string 1 line)))
				     man-result)))))
      (if match
	  (man match))))
   ;; Perl mode
   ((string= major-mode "cperl-mode")
    (cperl-perldoc-at-point))
   ;; Python mode
   ((string= major-mode "python-mode")
    (anaconda-mode-show-doc))
   ;; Emacs lisp mode
   ((or (string= major-mode "emacs-lisp-mode")
	(string= major-mode "lisp-interaction-mode"))
    (let* ((string (thing-at-point 'symbol))
	   (symbol (intern-soft string)))
      (when symbol
	(cond ((fboundp symbol)
	       (describe-function symbol))
	      ((boundp symbol)
	       (describe-variable symbol))))))))


(provide 'my-programming)
