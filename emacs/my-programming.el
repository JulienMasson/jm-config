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

;; auto-detection indenting
(require 'dtrt-indent)
(add-hook 'c-mode-common-hook
	  (lambda() (require 'dtrt-indent)
	    (dtrt-indent-mode t)))

;; change default grep
(setq grep-command "grep -nrH -e ")

;; el doc mode
(require 'c-eldoc)
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)


(provide 'my-programming)
