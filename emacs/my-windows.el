;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              WINDOWS CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; disable erase-buffer
(put 'erase-buffer 'disabled nil)

;; set some variables
(custom-set-variables
 '(c-default-style (quote ((awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

;; set font size
(set-face-attribute 'default nil :height 100)

;; load theme
(add-to-list 'custom-theme-load-path "~/config/emacs/modules/ample-zen")
(load-theme 'ample-zen t)

;; enable ido-mode
(ido-mode 1)

;; hide welcome screen
(setq inhibit-startup-message t)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; show parenthese
(show-paren-mode 1)

;; dired Extra
(add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

;; virtual desktop
(require 'virtual-desktops)
(virtual-desktops-mode 1)

;; edit file root
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; highlight focus
(require 'face-remap)
(defvar highlight-focus:last-buffer nil)
(defvar highlight-focus:cookie nil)
(defvar highlight-focus:background "#303030")
(defvar highlight-focus:app-has-focus t)

(defun highlight-focus:check ()
  "Check if focus has changed, and if so, update remapping."
  (let ((current-buffer (and highlight-focus:app-has-focus (current-buffer))))
    (unless (eq highlight-focus:last-buffer current-buffer)
      (when (and highlight-focus:last-buffer highlight-focus:cookie)
        (with-current-buffer highlight-focus:last-buffer
          (face-remap-remove-relative highlight-focus:cookie)))
      (setq highlight-focus:last-buffer current-buffer)
      (when current-buffer
        (setq highlight-focus:cookie
              (face-remap-add-relative 'default :background highlight-focus:background))))))

(defun highlight-focus:app-focus (state)
  (setq highlight-focus:app-has-focus state)
  (highlight-focus:check))

(defadvice other-window (after highlight-focus activate)
  (highlight-focus:check))
(defadvice select-window (after highlight-focus activate)
  (highlight-focus:check))
(defadvice select-frame (after highlight-focus activate)
  (highlight-focus:check))
(add-hook 'window-configuration-change-hook 'highlight-focus:check)

(add-hook 'focus-in-hook (lambda () (highlight-focus:app-focus t)))
(add-hook 'focus-out-hook (lambda () (highlight-focus:app-focus nil)))

;; remove scroll bar
(scroll-bar-mode -1)

;; load status
(require 'status)
(toggle-status)

;; browse kill ring
(require 'browse-kill-ring)

;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        KEYS SHORTCUTS        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; windows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;
(global-set-key (kbd "C-M-y") (lambda () (interactive) (yank-pop -1)))

;; goto virtual desktops
(global-set-key (kbd "M-<kp-1>") (lambda () (interactive) (virtual-desktops-goto 1)))
(global-set-key (kbd "M-<kp-2>") (lambda () (interactive) (virtual-desktops-goto 2)))
(global-set-key (kbd "M-<kp-3>") (lambda () (interactive) (virtual-desktops-goto 3)))
(global-set-key (kbd "M-<kp-4>") (lambda () (interactive) (virtual-desktops-goto 4)))
(global-set-key (kbd "M-<kp-5>") (lambda () (interactive) (virtual-desktops-goto 5)))
(global-set-key (kbd "M-<kp-6>") (lambda () (interactive) (virtual-desktops-goto 6)))
(global-set-key (kbd "M-<kp-7>") (lambda () (interactive) (virtual-desktops-goto 7)))
(global-set-key (kbd "M-<kp-8>") (lambda () (interactive) (virtual-desktops-goto 8)))
(global-set-key (kbd "M-<kp-9>") (lambda () (interactive) (virtual-desktops-goto 9)))

;; sudo-edit
(global-set-key (kbd "C-x C-r") 'sudo-edit)

;; shortcut for goto-line
(global-set-key (kbd "C-l") 'goto-line)

;; shortcut for revert buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;; shortcut for highlight a line with regex
(global-set-key (kbd "C-c h") 'highlight-lines-matching-regexp)

;; shortcut for comment region
(global-set-key (kbd "C-c m") 'comment-region)

;; shortcut for uncomment region
(global-set-key (kbd "C-c u") 'uncomment-region)

;; kill buffer and current windows
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)

;; find-name-dired
(global-set-key (kbd "C-c f") 'find-name-dired)

;; org timer shortcuts
(global-set-key (kbd "C-c t s") 'org-timer-start)
(global-set-key (kbd "C-c t p") 'org-timer-pause-or-continue)
(global-set-key (kbd "C-c t e") 'org-timer-stop)


(provide 'my-windows)
