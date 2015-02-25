;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;             KEY BINDINGS WORK             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; intel
(global-set-key (kbd "C-c i s") 'switch-project)
(global-set-key (kbd "C-c i p") 'project-compile)
(global-set-key (kbd "C-c SPC") (lambda () (interactive) (with-current-buffer "*compilation*" (recompile))))
(global-set-key (kbd "C-c C-f") 'project-find-file)
(global-set-key (kbd "C-c i d") 'device-contol)
(global-set-key (kbd "C-c i l") 'log-tools)
(global-set-key (kbd "C-c i t") 'toggle-window-dedicated)
(global-set-key (kbd "C-c i d") 'device-contol)
(global-set-key (kbd "C-c i u") 'repo-upload)


(provide 'work-keybindings)
