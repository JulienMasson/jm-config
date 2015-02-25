;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            KEY BINDINGS DEFAULT           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; windows
;; (global-set-key (kbd "C-M-k") 'windmove-left)
;; (global-set-key (kbd "C-M-l") 'windmove-up)
;; (global-set-key (kbd "C-M-'") 'windmove-down)
;; (global-set-key (kbd "C-M-;") 'windmove-right)

;; controls commands
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c S") 'sudo-edit)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c d") 'delete-rectangle)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c h") 'highlight-lines-matching-regexp)
(global-set-key (kbd "C-c m") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)
(global-set-key (kbd "C-c f") 'find-name-dired)

(global-set-key (kbd "C-M-y") (lambda () (interactive) (yank-pop -1)))

;; goto virtual desktops
(global-set-key (kbd "M-1") (lambda () (interactive) (virtual-desktops-goto 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (virtual-desktops-goto 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (virtual-desktops-goto 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (virtual-desktops-goto 4)))
(global-set-key (kbd "M-5") (lambda () (interactive) (virtual-desktops-goto 5)))
(global-set-key (kbd "M-6") (lambda () (interactive) (virtual-desktops-goto 6)))
(global-set-key (kbd "M-7") (lambda () (interactive) (virtual-desktops-goto 7)))
(global-set-key (kbd "M-8") (lambda () (interactive) (virtual-desktops-goto 8)))
(global-set-key (kbd "M-9") (lambda () (interactive) (virtual-desktops-goto 9)))


;; org timer shortcuts
(global-set-key (kbd "C-c t s") 'org-timer-start)
(global-set-key (kbd "C-c t p") 'org-timer-pause-or-continue)
(global-set-key (kbd "C-c t e") 'org-timer-stop)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)

;; wl
(global-set-key (kbd "C-c w u") 'mu-wl-update-database)
(global-set-key (kbd "C-c w c") 'mu-wl-update-contacts)

;; erc
(global-set-key (kbd "C-c i n") 'erc-channel-names)
(global-set-key (kbd "C-c i i") (lambda () (interactive)
			   (erc-tls :server "otcirc.jf.intel.com" :port "994"
				:nick "jmasson")))
(global-set-key (kbd "C-c i r") (lambda () (interactive)
			   (erc :server "irc.freenode.net" :port "6667"
				:nick "jmasson")))

;; shortcuts for pidgin using purple
(global-set-key (kbd "C-c p i") 'purple-init)
(global-set-key (kbd "C-c p b") 'purple-buddies-list)
(global-set-key (kbd "C-c p s") 'purple-status-set)
(global-set-key (kbd "C-c p m") 'purple-mail-to)
(global-set-key (kbd "C-c p a") 'purple-buddy-add)

;; magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)
(global-set-key (kbd "C-c g f") 'magit-file-log)
(global-set-key (kbd "C-c g p") 'magit-pull)
(global-set-key (kbd "C-c g r") 'magit-reset-head-hard)
(global-set-key (kbd "C-c g a") 'magit-commit-amend)
(global-set-key (kbd "C-c g b") 'magit-blame-mode)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g n") 'magit-create-branch)
(global-set-key (kbd "C-c g d") 'magit-delete-branch)
(global-set-key (kbd "C-c g m") 'magit-branch-manager)
(global-set-key (kbd "C-c g u") 'check-git-branch-update)

;; search with ctags or cscope
(global-set-key (kbd "M-;") 'find-tag)


(provide 'my-keybindings)
