;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto complete clang
;; (require 'auto-complete-clang)
;; (global-set-key (kbd "C-#") 'ac-complete-clang)

;; search with gtags
;; (setq helm-gtags-prefix-key "\C-cg")
;; (require 'setup-helm)
;; (require 'setup-helm-gtags)
(require 'setup-ggtags)

;; completion gtags
(require 'cc-mode)
(require 'company-gtags)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(setq company-backends '(company-elisp 
                         company-ropemacs
                         company-gtags
                         company-dabbrev-code
                         company-keywords
                         company-files 
                         company-dabbrev))
(define-key c-mode-map  [(control tab)] 'company-complete)
(define-key c++-mode-map  [(control tab)] 'company-complete)

;; info c-eldoc + gtags
;; (require 'c-eldoc)
;; (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
;; (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; enable yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

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

;; Enable Semantic
;; (semantic-mode 1)

;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

;; enable cedet cscope
;; (semanticdb-enable-cscope-databases)
;; (setq ede-locate-setup-options
;;       '(ede-locate-cscope
;; 	ede-locate-base))
;; (ede-cpp-root-project "Robot" :file "/home/jmassonx/Documents/OpenWide/test/Software/src/cscope.files")


(provide 'my-programming-test)
