;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                IRC CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; erc
(require 'erc)

;; erc notification
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/erc-nick-notify-0.2.1"))
(require 'erc-nick-notify)

;; shortcut to connect to intel irc
(global-set-key (kbd "C-c i i") (lambda () (interactive)
			   (erc-tls :server "otcirc.jf.intel.com" :port "994"
				:nick "jmasson")))

;; shortcut to connect to RoseTeam irc
(global-set-key (kbd "C-c i r") (lambda () (interactive)
			   (erc :server "irc.freenode.net" :port "6667"
				:nick "jmasson")))

;; join the #pupdr whenever connecting to intel irc.
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("otcirc.jf.intel.com" "#pupdr")
	("irc.freenode.net" "#RoseTeam")))

;; shortcuts for magit
(global-set-key (kbd "C-c i n") 'erc-channel-names)
