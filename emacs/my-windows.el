;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              WINDOWS CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vertical split by default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; windows config
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
(setq shift-select-mode t)

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

;; ansi color
(require 'comint)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)


(provide 'my-windows)
