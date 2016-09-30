;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               CONFIG EMACS                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs config is set to nil by default
(setq jm-config-emacs nil)

;; add my config path
(add-to-list 'load-path "~/jm-config/emacs/")
(let ((default-directory "~/jm-config/emacs/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'my-config)
