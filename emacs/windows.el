;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              WINDOWS CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(put 'erase-buffer 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set font size
(set-face-attribute 'default nil :height 100)

;; load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/ample-zen/")
(load-theme 'ample-zen t)

;; enable ido-mode
(ido-mode 1)

;; hide welcome screen
(setq inhibit-startup-message t)

;; windows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; show parenthese
(show-paren-mode 1)

;; dired Extra
(add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

;;
(global-set-key (kbd "C-M-y") (lambda () (interactive) (yank-pop -1)))

;; virtual desktop
(load "~/.emacs.d/elpa/virtual-desktops.el/virtual-desktops.el")
(virtual-desktops-mode 1)

;; clear shell screen
(defun my-clear ()
      (interactive)
      (erase-buffer)
      (comint-send-input))
(defun my-shell-hook ()
  (local-set-key (kbd "C-c l") 'my-clear))
  (add-hook 'shell-mode-hook 'my-shell-hook)

;; edit file root
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)
