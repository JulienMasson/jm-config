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

;; irc config
(require 'my-irc)

;; programming config
(require 'my-programming)

;; shell config
(require 'my-shell)

;; github config
(require 'my-github)

;; rss config
(require 'my-rss)

;; work config
(require 'my-work)

;; keybindings config
(require 'my-keybindings)

;; pdf tools
(require 'pdf-tools)
(pdf-tools-install)


(provide 'my-config)
