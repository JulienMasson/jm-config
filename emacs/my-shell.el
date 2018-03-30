;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;               SHELL CONFIG                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set default shell
(setq explicit-shell-file-name "/bin/bash")

;; ssh history
(defvar ssh-history '()
  "History of `ssh' function argument.")

(defun ssh (&optional login-host)
  "Create a new ssh shell on HOST."
  (interactive (list (read-string "login@host: " nil 'ssh-history)))
  (with-current-buffer (get-buffer-create (concat "*" login-host "*"))
    (setq default-directory (concat "/ssh:" login-host ":"))
    (shell (current-buffer))
    ;; Input ring bash history
    (set (make-local-variable 'comint-input-ring-file-name)
	 (concat "/ssh:" login-host ":~/.bash_history"))
    (comint-read-input-ring)))

;; clear shell screen
(defun my-clear ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(defun my-shell-hook ()
  (local-set-key (kbd "C-c l") 'my-clear))

(add-hook 'shell-mode-hook 'my-shell-hook)

;; history results
(defun search-history (pattern number)
  "Search in history"
  (interactive "sEnter pattern: \nnTail: ")
  (with-output-to-temp-buffer "*search-history-output*"
    (shell-command (format "cat ~/.zsh_history | grep %s | cut -c 16- | tail -n %d" pattern number)
                   "*search-history-output*"
                   "*Messages*")
    (pop-to-buffer "*search-history-output*")))

;; export PS1
(defun export-ps1-shell ()
  (let ((process (get-buffer-process (current-buffer))))
    (goto-char (process-mark process))
    (process-send-string
     process (format "export PS1='\\[\\e[1;34m\\]\\u  \\w${text}\\[\\e[m\\]\\n\\[\\e[1;32m\\]\\t\\[\\e[m\\] '&&"))
    (while (accept-process-output process 0.1))))

(add-hook 'shell-mode-hook 'export-ps1-shell)


(provide 'my-shell)
