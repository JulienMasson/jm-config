;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                HOME CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mail
(require 'home-gnus)

;; Status
(require 'status)
(status-add-to-left 'status-virtual-desktops)
(status-add-to-right 'status-gnus)
(turn-on-status)

(provide 'my-home)
