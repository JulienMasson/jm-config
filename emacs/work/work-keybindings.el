;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             KEY BINDINGS WORK             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; intel
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c SPC") (lambda () (interactive) (with-current-buffer "*compilation*" (recompile))))
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "C-c i d") 'device-contol)
(global-set-key (kbd "C-c i l") 'log-tools)
(global-set-key (kbd "C-c i t") 'toggle-window-dedicated)
(global-set-key (kbd "C-c i d") 'device-contol)
(global-set-key (kbd "C-c C-o") 'browse-url)

;; repo
(global-set-key (kbd "C-c r u") 'repo-upload)
(global-set-key (kbd "C-c r b") 'repo-start)
(global-set-key (kbd "C-c r s") 'repo-status)
(global-set-key (kbd "C-c r g") 'repo-sync-local)
(global-set-key (kbd "C-c r G") 'repo-sync)


(provide 'work-keybindings)
