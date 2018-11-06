;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              WINDOWS CONFIG               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vertical split by default
(setq split-height-threshold 119)
(setq split-width-threshold 143)

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
(ido-everywhere)

;; disable automatic file search in ido mode
(setq ido-auto-merge-work-directories-length -1)

;; ido-completing-read+
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; ido on yes-or-no
(defun ido-yes-or-no-p (prompt)
  (let* ((yes-or-no-prompt (concat prompt " "))
         (choices '("YES" "NO"))
         (answer (ido-completing-read yes-or-no-prompt choices
				      nil t nil nil)))
    (string= answer "YES")))

(defadvice yes-or-no-p (around use-ido activate)
  (setq ad-return-value (ido-yes-or-no-p prompt)))

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

;; load jm theme
(add-to-list 'custom-theme-load-path "~/jm-config/emacs")
(load-theme 'jm t)

;; change background color on focus window
(defadvice other-window (after highlight-focus activate)
  (highlight-focus:check))
(defadvice select-window (after highlight-focus activate)
  (highlight-focus:check))
(defadvice select-frame (after highlight-focus activate)
  (highlight-focus:check))
(add-hook 'window-configuration-change-hook 'highlight-focus:check)
(add-hook 'focus-in-hook (lambda () (highlight-focus:app-focus t)))
(add-hook 'focus-out-hook (lambda () (highlight-focus:app-focus nil)))

;; line highlighting in all buffers
(require 'hl-line)
(global-hl-line-mode 1)

;; remove scroll bar
(scroll-bar-mode -1)

;; modeline
(require 'telephone-line)
(setq telephone-line-lhs
      '((accent . (telephone-line-airline-position-segment))
        (nil    . (telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment
		   telephone-line-process-segment))
        (accent . (telephone-line-major-mode-segment))))
(telephone-line-mode 1)

;; ansi color
(require 'comint)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

;; silent some message in echo area
(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'message #'silence))))

(dolist (func '(isearch-done
		undo
		basic-save-buffer
		push-mark))
  (if (functionp func)
      (advice-add func :around #'suppress-messages)))

(defun my-command-error-function (data context caller)
  (when (not (memq (car data) '(do-auto-save
				buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;; remove eldoc mode
(global-eldoc-mode -1)

;; silent tramp message
(setq tramp-verbose 1)

;; ansi color buffer
(defun ansi-color-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; window dedicated
(defun toggle-window-dedicated ()
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s dedicated"
     "%s free")
   (current-buffer)))

;; toggle horizontal split
(defun toggle-horizontal-split ()
  (interactive)
  (if (not split-height-threshold)
      (setq split-height-threshold 119)
    (setq split-height-threshold nil)))

;; hide lines
(require 'hide-lines)

;; move lines
(require 'move-lines)
(move-lines-binding)


(provide 'my-windows)
