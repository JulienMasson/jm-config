;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               SHELL CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clear shell screen
(defun my-clear ()
      (interactive)
      (erase-buffer)
      (comint-send-input))
(defun my-shell-hook ()
  (local-set-key (kbd "C-c l") 'my-clear))
  (add-hook 'shell-mode-hook 'my-shell-hook)
