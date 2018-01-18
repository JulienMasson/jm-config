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

;; org timer shortcuts
(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)
(global-set-key (kbd "C-c o c") 'org-capture)

;; magit
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-head)
(global-set-key (kbd "C-c g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g r") 'magit-reset-hard)
(global-set-key (kbd "C-c g b") 'magit-blame)

;; compilation
(global-set-key (kbd "C-c SPC") (lambda () (interactive) (with-current-buffer "*compilation*" (recompile))))

;; open browser
(global-set-key (kbd "C-c C-o") 'browse-url)

;; refresh status
(global-set-key (kbd "C-c C-u") (lambda () (interactive) (run-at-time 1 status-refresh-timer-delay 'status-update)))

;; bitlbee
(global-set-key (kbd "C-c e b") 'bitlbee-update-list)
(global-set-key (kbd "C-c e c") 'bitlbee-chat)
(global-set-key (kbd "C-c e j") 'bitlbee-jump)

;; cscope
(define-key cscope-minor-mode-keymap  "\C-csa" 'cscope-add-cscope-search-list)
(define-key cscope-minor-mode-keymap  "\C-csr" 'cscope-reset-cscope-search-list)

;; map TAB to company completion in shell mode
(define-key shell-mode-map (kbd "TAB") #'company-manual-begin)

;; pycscope
(define-key cscope-minor-mode-keymap (kbd "C-c s p") 'cscope-pycscope)

;; manual at point
(global-set-key (kbd "M-h") 'manual-at-point)

;; mu4e
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

;; project manager
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c f") 'project-find-file-subproject)


(provide 'my-keybindings)
