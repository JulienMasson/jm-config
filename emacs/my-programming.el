;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             PROGRAMMING CONFIG            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; jump only on error compilation
(setq compilation-skip-threshold 2)

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

;; auto complete config
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/jm-config/auto-complete/dict/")
(ac-config-default)

;; auto complete based on ggtags
(require 'setup-ggtags)
(set-face-foreground 'ac-gtags-candidate-face "coral3")
(set-face-background 'ac-gtags-selection-face nil)


(provide 'my-programming)
