;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;            KEY BINDINGS DEFAULT           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; controls commands
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c d") 'delete-rectangle)
(global-set-key (kbd "C-c M-i") 'string-insert-rectangle)
(global-set-key (kbd "C-c M-d") 'delete-matching-lines)
(global-set-key (kbd "C-c M-r") 'revert-buffer)
(global-set-key (kbd "C-c h") 'highlight-lines-matching-regexp)
(global-set-key (kbd "C-c m") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)
(global-set-key (kbd "C-c M-f") 'find-name-dired)
(global-set-key (kbd "C-M-y") (lambda () (interactive) (yank-pop -1)))
(global-set-key (kbd "C-c M-w") 'show-and-copy-buffer-filename)

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
(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)
(global-set-key (kbd "C-c o c") 'org-capture)

;; shortcuts for pidgin using purple
(global-set-key (kbd "C-c p i") 'purple-init)
(global-set-key (kbd "C-c p b") 'purple-buddies-list)
(global-set-key (kbd "C-c p s") 'purple-status-set)
(global-set-key (kbd "C-c p m") 'purple-mail-to)
(global-set-key (kbd "C-c p a") 'purple-buddy-add)
(global-set-key (kbd "C-c p j") 'purple-chat-jump)

;; perl
(global-set-key (kbd "C-c p d") 'perldoc)

;; magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-head)
(global-set-key (kbd "C-c g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g p") 'magit-pull)
(global-set-key (kbd "C-c g r") 'magit-reset-hard)
(global-set-key (kbd "C-c g a") 'magit-commit-amend)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g n") 'magit-create-branch)
(global-set-key (kbd "C-c g d") 'magit-delete-branch)
(global-set-key (kbd "C-c g m") 'magit-branch-manager)
(global-set-key (kbd "C-c g u") 'check-git-branch-update)

;; xcsope
(global-set-key (kbd "C-c c d") 'jm-cscope-database)
(global-set-key (kbd "C-c c s") 'jm-switch-cscope-database)

;; locate
(global-set-key (kbd "C-c C-l") 'jm-search-locate-database)



(provide 'my-keybindings)
