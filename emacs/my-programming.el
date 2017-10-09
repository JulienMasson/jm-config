;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump only on error compilation
(setq compilation-skip-threshold 2)

;; visual regexp
(require 'visual-regexp)

;; markdown mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; default indentation
(setq c-default-style (quote ((awk-mode . "awk") (c-mode . "linux") (other . "gnu"))))

;; save backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; use uncrustify with dired mode
(setq uncrustify-config-path "~/bin/uncrustify/etc/linux.cfg")
(setq uncrustify-bin "~/bin/uncrustify/src/uncrustify")
(setq uncrustify-args "--no-backup")

(defun uncrustify-dired ()
  (interactive)
  (setq uncrustify-files (mapconcat 'identity
				    (mapcar 'untramp-path (dired-get-marked-files))
				    " "))
  (if (not (string= uncrustify-files ""))
      (shell-command (format "%s -c %s %s %s"
			     uncrustify-bin
			     uncrustify-config-path
			     uncrustify-args
			     uncrustify-files))
    (message (propertize "No files selected" 'face 'error))))

;; lua mode
(require 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; cscope
(require 'xcscope)
(setq cscope-option-do-not-update-database t)
(setq cscope-option-use-inverted-index t)
(cscope-setup)
(setq path-to-cscope "/usr/bin/cscope")
(defun create-tags-cscope (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "cd %s; find . -name \"*.c\" -o -name \"*.cpp\" -o -name \"*.h\" > cscope.files; %s -q -R -b -i cscope.files" (directory-file-name dir-name) path-to-cscope)))

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

(define-key cscope-minor-mode-keymap  "\C-csa" 'cscope-add-cscope-search-list)
(define-key cscope-minor-mode-keymap  "\C-csr" 'cscope-reset-cscope-search-list)


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

;; map TAB to company completion in shell mode
(define-key shell-mode-map (kbd "TAB") #'company-manual-begin)

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


(provide 'my-programming)
