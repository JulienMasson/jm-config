;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                IRC CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; erc
(require 'erc)

;; erc notification
(add-to-list 'erc-modules 'notifications)

;; autojoin
(require 'erc-join)
(erc-autojoin-enable)
(setq erc-autojoin-channels-alist
      '(("otcirc.jf.intel.com" "#pupdr")
	("irc.freenode.net" "#RoseTeam")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYS SHORTCUTS        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c i n") 'erc-channel-names)
(global-set-key (kbd "C-c i i") (lambda () (interactive)
			   (erc-tls :server "otcirc.jf.intel.com" :port "994"
				:nick "jmasson")))
(global-set-key (kbd "C-c i r") (lambda () (interactive)
			   (erc :server "irc.freenode.net" :port "6667"
				:nick "jmasson")))

(provide 'my-irc)
