;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; search with ctags or cscope
(global-set-key (kbd "M-;") 'find-tag)

;; el doc mode
(require 'c-eldoc)
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; ;; auto-complete
;; (defun init-auto-complete ()
;;   (require 'auto-complete-config)
;;   (add-to-list 'ac-dictionary-directories (expand-file-name "~/jm-config/emacs/modules/auto-complete/dict"))
;;   (ac-config-default)
;;   ;; (add-to-list 'ac-modes 'shell-mode)
;;   (ac-etags-setup))

;; ;; after-init
;; (defun after-init ()
;;   (init-auto-complete)
;;   )
;; (add-hook 'after-init-hook 'after-init)

;; auto-detection indenting
(require 'dtrt-indent)
(add-hook 'c-mode-common-hook
	  (lambda() (require 'dtrt-indent)
	    (dtrt-indent-mode t)))

;; change default grep
(setq grep-command "grep -nrH -e ")

;; add apt-utils
(require 'apt-utils)
(require 'apt-utils-ido)

;; add ac-c-headers
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))


(provide 'my-programming)
