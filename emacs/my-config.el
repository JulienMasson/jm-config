;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             MAIN CONFIG EMACS             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; windows config
(require 'my-windows)

;; tramp config
(require 'my-tramp)

;; org config
(require 'my-org)

;; dired config
(require 'my-dired)

;; erc config
(require 'my-erc)

;; programming config
(require 'my-programming)

;; shell config
(require 'my-shell)

;; github config
(require 'my-github)

;; rss config
(require 'my-rss)

;; keybindings config
(require 'my-keybindings)

;; pdf tools
(require 'pdf-tools)
(pdf-tools-install)
;; jm private
(require 'jm-private)


(provide 'my-config)
