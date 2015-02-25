;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump only on error compilation
(setq compilation-skip-threshold 2)

;; ctags
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s" path-to-ctags dir-name (directory-file-name dir-name))))

;; cscope
(require 'xcscope)
(cscope-setup)
(setq path-to-cscope "/usr/bin/cscope")
(defun create-tags-cscope (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "cd %s; find . -name \"*.c\" -o -name \"*.cpp\" -o -name \"*.h\" > cscope.files; %s -q -R -b -i cscope.files" (directory-file-name dir-name) path-to-cscope)))

;; auto-detection indenting
(require 'dtrt-indent)
(add-hook 'c-mode-common-hook
	  (lambda() (require 'dtrt-indent)
	    (dtrt-indent-mode t)))

;; change default grep
(setq grep-command "grep -nrH -e ")

;; el doc mode
;; (require 'c-eldoc)
;; (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; enable yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; auto complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/jm-config/auto-complete/dict/")
(ac-config-default)

;; auto complete clang
(require 'auto-complete-clang)
(global-set-key (kbd "C-#") 'ac-complete-clang)

;; ;; cedet
;; (require 'cedet)
;; (require 'cedet-cscope)
;; (require 'semantic)
;; (require 'semantic/symref/cscope)
;; (require 'semantic/db-cscope)
;; ;;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;; ;; Enable Semantic
;; (semantic-mode 1)

;; ;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

;; ;; enable cedet cscope
;; (semanticdb-enable-cscope-databases)
;; (setq ede-locate-setup-options
;;       '(ede-locate-cscope
;; 	ede-locate-base))
;; (ede-cpp-root-project "Robot" :file "/home/jmassonx/Documents/Software/src/cscope.files")


(provide 'my-programming)
