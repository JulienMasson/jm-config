;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                PERL CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; perl doc
(require 'perldoc)

;; interactive perl shell
(require 'inf-perl)

;; cperl-mode is preferred to perl-mode                                        
(defalias 'perl-mode 'cperl-mode)


(provide 'my-perl)
