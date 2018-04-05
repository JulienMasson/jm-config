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
(setq erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)
(setq erc-log-insert-log-on-open t)

;; apply face on log
(defun erc-log-match-face ()
  (list
   ;; own message line
   `(,(format "^\\(<\\)\\(%s\\)\\(>\\)[ \n]\\(%s\\)$" "jmasson" erc-view-log-message-regexp)
     (1 'erc-default-face)
     (2 'erc-my-nick-face)
     (3 'erc-default-face)
     (4 'erc-input-face))
   ;; standard message line
   `(,(format "^\\(<\\)\\(%s\\)\\(>\\)[ \n]\\(%s\\)$" erc-view-log-nickname-regexp erc-view-log-message-regexp)
     (1 'erc-default-face)
     (2 (erc-log-nick-get-face (match-string 3)))
     (3 'erc-default-face)
     (4 'erc-default-face))
   ;; *** message line
   `("^\\(\\*\\*\\*.*\\)"
     (1 'erc-timestamp-face))
   ;; [date] message line
   `("^\\(\\[.*\\]\\)"
     (1 'erc-timestamp-face))))

(defun erc-apply-face-on-log ()
  (setq font-lock-defaults `(,(erc-log-match-face))))

(advice-add 'erc-mode :before #'erc-apply-face-on-log)


(provide 'my-erc)
