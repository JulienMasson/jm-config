;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ERC CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; erc
(require 'erc)

;; erc notification
(add-to-list 'erc-modules 'notifications)

;; erc chat jump
(defun erc-chat-jump (buffer)
  (interactive (list (ido-completing-read "Switch to ERC buffer: "
					  (mapcar (lambda (arg) (buffer-name (car arg))) erc-modified-channels-alist)
					  nil t nil nil)))
  (switch-to-buffer buffer))

;; erc image
(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

;; emojify mode
(require 'emojify)
(add-hook 'erc-mode-hook 'emojify-mode)

;; exclude some types of messages
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"))

;; logging
(erc-log-mode)
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-generate-log-file-name-function (quote erc-generate-log-file-name-with-date))
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)
(setq erc-log-insert-log-on-open t)

;; erc view log
(require 'erc-view-log)
(add-to-list 'auto-mode-alist
	     `(,(format "%s/.*\\.log"
			(regexp-quote (expand-file-name erc-log-channels-directory))) . erc-view-log-mode))
(add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)


(provide 'my-erc)
