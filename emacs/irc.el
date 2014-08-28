;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                IRC CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; erc
(require 'erc)

;; shortcut to connect to intel irc
(global-set-key "\C-cef" (lambda () (interactive)
			   (erc-tls :server "otcirc.jf.intel.com" :port "994"
				:nick "jmasson")))

;; join the #pupdr whenever connecting to intel irc.
(setq erc-autojoin-channels-alist '(("otcirc.jf.intel.com" "#pupdr")))
