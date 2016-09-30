;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                TRAMP CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; override tramp histfile
(setq tramp-histfile-override "$HOME/.tramp_history")

;; expand tramp remote path
(require 'tramp-sh)
(add-to-list 'tramp-remote-path "~/bin")
(add-to-list 'exec-path "~/bin")

;; auto revert remote files
(setq auto-revert-remote-files t)

;; untramp path
(defun untramp-path (path)
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))


(provide 'my-tramp)
