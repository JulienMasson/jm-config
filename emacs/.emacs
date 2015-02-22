;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               CONFIG EMACS                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add my config path
(add-to-list 'load-path "~/jm-config/emacs/")
(let ((default-directory "~/jm-config/emacs/"))
  (normal-top-level-add-subdirs-to-load-path))

;; add intel config path
(add-to-list 'load-path "~/Documents/Intel/intel-lisp/")
(let ((default-directory "~/Documents/Intel/intel-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'my-config)
