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



(provide 'my-irc)
