;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              WINDOWS CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; vertical split by default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; don't ask to follow symlink
(setq vc-follow-symlinks t)

;; disable erase-buffer
(put 'erase-buffer 'disabled nil)

;; delete recursively without asking
(setq dired-recursive-deletes 'always)

;; windows config
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
(setq shift-select-mode t)

;; save password
(setq password-cache-expiry nil)

;; set some variables
(custom-set-variables
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

;; windows size
(setq default-frame-alist '((width . 80) (height . 40)))

;; set font size
(set-face-attribute 'default nil :height 90)

;; enable ido-mode
(require 'ido)
(setq ido-ignore-files '("\\`\\.\\./" "\\`\\./"))
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

;; default dired setting
(put 'dired-find-alternate-file 'disabled nil)

;; virtual desktop
(require 'virtual-desktops)
(setq virtual-desktops-display-mode-line nil)
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

;; Graphical / Console - theme
(add-to-list 'custom-theme-load-path "~/jm-config/emacs")
(if (display-graphic-p)
      (progn
	(load-theme 'jm t)
	(defadvice other-window (after highlight-focus activate)
	  (highlight-focus:check))
	(defadvice select-window (after highlight-focus activate)
	  (highlight-focus:check))
	(defadvice select-frame (after highlight-focus activate)
	  (highlight-focus:check))
	(add-hook 'window-configuration-change-hook 'highlight-focus:check)
	(add-hook 'focus-in-hook (lambda () (highlight-focus:app-focus t)))
	(add-hook 'focus-out-hook (lambda () (highlight-focus:app-focus nil))))
  (progn
    (require 'color-theme-sanityinc-tomorrow)
    (color-theme-sanityinc-tomorrow--define-theme eighties)))

;; remove scroll bar
(scroll-bar-mode -1)

;; which key
(require 'which-key)
(which-key-mode)

;; smart mode line
(require 'smart-mode-line)
(setq sml/theme 'dark
      sml/theme 'light
      sml/theme 'respectful
      sml/no-confirm-load-theme t)
(sml/setup)

;; buffer move
(require 'buffer-move)

;; copy buffer filename
(defun show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))


(provide 'my-windows)
