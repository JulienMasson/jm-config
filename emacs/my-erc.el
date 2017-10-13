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


(provide 'my-erc)
