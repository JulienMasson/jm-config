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
(global-set-key (kbd "C-c M-i") 'string-rectangle)
(global-set-key (kbd "C-c M-d") 'delete-matching-lines)
(global-set-key (kbd "C-c M-r") 'revert-buffer)
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

;; org
(global-set-key (kbd "C-c o a") 'jm-org-agenda)

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
(global-set-key (kbd "C-c e b") 'bitlbee-update-all)
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
(global-set-key (kbd "C-M-m") 'mu4e)
(global-set-key (kbd "C-M-u") (lambda () (interactive)
                                (mu4e-headers-search-bookmark "flag:unread AND NOT flag:trashed")))
(define-key mu4e-view-mode-map (kbd "C-c C-s") 'mu4e-ido-save-attachments)

;; project manager
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c f") 'project-find-file-subproject)

;; grep at point
(global-set-key (kbd "C-M-g") 'grep-at-point)

;; grep save buffer
(define-key grep-mode-map "s" 'grep-save-buffer)

;; window dedicated
(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;; hide lines
(global-set-key (kbd "C-c h") 'hide-lines)

;; anaconda
(define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
(define-key anaconda-mode-map (kbd "M-=") 'anaconda-mode-find-assignments)

;; shell command with editor
(global-set-key (kbd "M-&") 'with-editor-async-shell-command)
(global-set-key (kbd "M-!") 'with-editor-shell-command)

;; translate at point
(global-set-key (kbd "M-*") 'translate-at-point)

;; dired
(define-key dired-mode-map "=" 'dired-diff-files)
(define-key dired-mode-map "r" 'dired-diff-directories)
(define-key dired-mode-map "h" 'dired-do-hexl-find-file)
(define-key dired-mode-map (kbd "^")
  (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map "h" 'dired-do-hexl-find-file)
(define-key dired-mode-map "L" 'locate-database)

;; erc
(define-key erc-mode-map (kbd "C-c C-i") 'emojify-insert-emoji)


(provide 'my-keybindings)
