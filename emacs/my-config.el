;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             MAIN CONFIG EMACS             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; windows config
(require 'my-windows)

;; org config
(require 'my-org)

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

;; home - work config
(if (string= jm-config-emacs "home")
    (require 'my-home)
  (if (string= jm-config-emacs "work")
      (require 'my-work)))

;; keybindings config
(require 'my-keybindings)


(provide 'my-config)
