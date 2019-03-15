;;; jm-theme.el --- My Custom Theme from zenburn

;; Copyright (C) 2019 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; URL: https://github.com/JulienMasson/jm-config/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme jm "Julien Masson color theme")

(defvar jm-colors-alist
  '(("background" . "#212121")
    ("black"      . "#000000")
    ("blue"       . "#528fd1")
    ("blue-dark"  . "#6380b3")
    ("brown"      . "#7d7c61")
    ("foreground" . "#bdbdb3")
    ("forest"     . "#6a7550")
    ("gray"       . "#3b3b3b")
    ("gray-dark"  . "#2e2e2e")
    ("gray-light" . "#9b9b9b")
    ("green"      . "#6aaf50")
    ("green-dark" . "#6a9550")
    ("orange"     . "#fb8512")
    ("purple"     . "#9b55c3")
    ("red"        . "#CC5542")
    ("slate"      . "#2f4f4f")
    ("yellow"     . "#baba36")))

(defmacro jm-with-color-variables (&rest body)
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   jm-colors-alist))
     ,@body))

(jm-with-color-variables
  (custom-theme-set-faces
   'jm
;;;; global
   `(cursor ((t (:foreground ,foreground :background ,orange))))
   `(default ((t (:foreground ,foreground :background ,background))))
   `(error ((t (:foreground ,red :weight bold))))
   `(fringe ((t (:foreground ,foreground :background ,background))))
   `(header-line ((t (:foreground ,brown :background ,gray-dark
				  :box (:line-width -1 :style released-button)))))
   `(link ((t (:foreground ,brown :underline t :weight bold))))
   `(match ((t (:background ,gray-dark :foreground ,orange :weight bold))))
   `(minibuffer-prompt ((t (:foreground ,brown :weight bold))))
   `(region ((,class (:background ,gray))))
   `(success ((t (:foreground ,green :weight bold))))
   `(trailing-whitespace ((t (:background ,red))))
   `(vertical-border ((t (:background ,black :foreground ,black))))
   `(warning ((t (:foreground ,orange :weight bold))))

;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,foreground :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,green))))
   `(font-lock-constant-face ((t (:foreground ,forest))))
   `(font-lock-doc-face ((t (:foreground ,green-dark))))
   `(font-lock-function-name-face ((t (:foreground ,purple))))
   `(font-lock-keyword-face ((t (:foreground ,brown :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,blue-dark))))
   `(font-lock-string-face ((t (:foreground ,red))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,orange))))
   `(font-lock-warning-face ((t (:foreground ,yellow :weight bold))))

;;;; mode-line
   `(mode-line ((,class (:foreground ,gray :background ,black
				     :box (:line-width -1 :style released-button)))
		(t :inverse-video t)))
   `(mode-line-buffer-id ((t (:inherit font-lock-variable-name-face :weight bold))))
   `(mode-line-inactive ((t (:foreground ,gray-light :background ,gray-dark
   					 :box nil :weight light))))

;;;; hl-line-mode
   `(hl-line ((,class (:background ,gray-dark))
              (t :weight bold)))
   `(hl-line-face ((,class (:background ,gray-dark))
                   (t :weight bold)))

;;;; ido-mode
   `(ido-first-match ((t (:inherit font-lock-variable-name-face :weight bold))))
   `(ido-incomplete-regex ((t (:foreground ,red))))
   `(ido-only-match ((t (:inherit font-lock-comment-face :weight bold))))
   `(ido-subdir ((t (:inherit font-lock-type-face))))

;;;; show-paren
   `(show-paren-match ((t (:foreground ,blue :background ,background :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,red :background ,background :weight bold))))

;;;; isearch
   `(isearch ((t (:foreground ,yellow :background ,background :weight bold))))
   `(lazy-highlight ((t (:inherit isearch))))

;;;; compilation
   `(compilation-info ((t (:inherit font-lock-constant-face :underline t))))
   `(compilation-mode-line-exit ((t (:inherit font-lock-doc-face :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
   `(compilation-mode-line-run ((t (:inherit font-lock-keyword-face))))

;;;; company
   `(company-preview ((t (:background ,gray))))
   `(company-preview-common ((t (:inherit company-preview :foreground ,orange))))
   `(company-scrollbar-bg ((t (:inherit company-tooltip))))
   `(company-scrollbar-fg ((t (:inherit company-tooltip-selection))))
   `(company-tooltip ((t (:background ,gray))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground ,orange))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground ,orange))))
   `(company-tooltip-selection ((t (:background ,gray-dark))))

;;;; diff
   `(diff-added ((t (:foreground ,forest :background nil))))
   `(diff-file-header ((t (:background ,black :bold t))))
   `(diff-header ((t (:background ,black))))
   `(diff-removed ((t (:foreground ,red :background nil))))

;;;; magit
   `(magit-diff-added ((t :inherit diff-added)))
   `(magit-diff-added-highlight ((t :inherit diff-added)))
   `(magit-diff-context-highlight ((t)))
   `(magit-diff-file-heading ((t)))
   `(magit-diff-hunk-heading-highlight ((t)))
   `(magit-diff-removed ((t :inherit diff-removed)))
   `(magit-diff-removed-highlight ((t :inherit diff-removed)))
   `(magit-header ((t (:inherit font-lock-keyword-face))))
   `(magit-log-date ((t :foreground ,slate)))
   `(magit-section-heading ((t :inherit font-lock-keyword-face)))
   `(magit-section-title ((t (:inherit font-lock-keyword-face))))

;;;; cscope
   `(cscope-file-face ((t (:inherit font-lock-constant-face))))
   `(cscope-function-face ((t (:inherit font-lock-type-face))))
   `(cscope-line-number-face ((t :foreground ,red)))
   `(cscope-separator-face ((t :bold t :overline t :underline t :foreground ,orange)))

;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-builtin-face))))
   `(message-header-bcc ((t (:inherit font-lock-variable-name-face))))
   `(message-header-cc ((t (:inherit font-lock-variable-name-face))))
   `(message-header-from ((t (:inherit font-lock-variable-name-face))))
   `(message-header-name ((t (:inherit font-lock-comment-face :bold t))))
   `(message-header-other ((t (:inherit font-lock-type-face))))
   `(message-header-subject ((t (:inherit font-lock-builtin-face))))
   `(message-header-to ((t (:inherit font-lock-variable-name-face))))
   `(message-separator ((t (:inherit font-lock-comment))))

;;;;; notmuch
   `(notmuch-tree-match-author-face ((t (:inherit font-lock-variable-name-face))))
   `(notmuch-tree-match-date-face ((t (:inherit font-lock-comment-face))))

;;;; org-mode
   `(org-agenda-date-today ((t (:foreground "white" :slant italic :weight bold))))
   `(org-agenda-structure ((t (:inherit font-lock-comment-face))))
   `(org-date ((t (:foreground ,blue :underline t))))
   `(org-done ((t (:bold t :weight bold :foreground ,green-dark))))
   `(org-level-1 ((t (:foreground ,orange))))
   `(org-level-2 ((t (:foreground ,forest))))
   `(org-level-3 ((t (:foreground ,blue))))
   `(org-level-4 ((t (:foreground ,yellow))))
   `(org-link ((t (:foreground ,yellow :underline t))))
   `(org-table ((t (:foreground ,green-dark))))
   `(org-todo ((t (:bold t :foreground ,red :weight bold))))
   ))


(provide-theme 'jm)
