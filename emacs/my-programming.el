;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump only on error compilation
(setq compilation-skip-threshold 2)

;; gdb config
(require 'load-relative)
(require 'loc-changes)
(require 'realgud)
(setq realgud-safe-mode nil)

(defun gdb (file)
  (interactive (list (ido-read-file-name "gdb on: ")))
  (realgud:gdb (concat "gdb " file)))

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
(cscope-setup)

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
(setq grep-command "grep --color -nsrH -e ")

;; ripgrep
(require 'rg)

;; company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; additionnals company backends
(require 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-c-headers)))

;; perl
(defalias 'perl-mode 'cperl-mode)

;; ctags
(defun create-ctags-tag (dir)
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (shell-command "/usr/bin/ctags -e -R .")))

;; python doc
(require 'pydoc)

;; pycscope
(defun cscope-pycscope (dir)
  (interactive "DDirectory: ")
  (let ((default-directory dir))
    (async-shell-command "pycscope -D .")))

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
    (pydoc-at-point))
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
