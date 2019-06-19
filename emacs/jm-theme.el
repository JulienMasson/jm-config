;;; jm-theme.el --- My Custom Theme inspired from Zenburn Theme

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

(defvar theme-colors-alist
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
    ("primary"    . "#fb8512")
    ("purple"     . "#9b55c3")
    ("red"        . "#CC5542")
    ("salmon"     . "#ee8262")
    ("slate"      . "#2f4f4f")
    ("white"      . "#ffffff")
    ("yellow"     . "#baba36")))

(defmacro theme-colors-with (&rest body)
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   theme-colors-alist))
     ,@body))

(theme-colors-with
  (custom-theme-set-faces
   'jm
;;;; default
   `(default ((t (:foreground ,foreground :background ,background))))

;;;; status
   `(success ((t (:foreground ,green :weight bold))))
   `(warning ((t (:foreground ,orange :weight bold))))
   `(error ((t (:foreground ,red :weight bold))))

;;;; region
   `(region ((,class (:background ,gray :weight bold))
	     (t :inverse-video t)))
;;;; cursor
   `(cursor ((t (:foreground ,foreground :background ,primary))))

;;;; border
   `(fringe ((t (:foreground ,foreground :background ,background))))
   `(vertical-border ((t (:background ,black :foreground ,black))))

;;;; header
   `(header-line ((t (:foreground ,brown :background ,gray-dark
				  :box (:line-width -1 :style released-button)))))

;;;; match
   `(match ((t (:background ,gray-dark :foreground ,primary :weight bold))))

;;;; whitespace
   `(trailing-whitespace ((t (:background ,red))))

;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,foreground :weight bold))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,green))))
   `(font-lock-comment-face ((t (:foreground ,green))))
   `(font-lock-constant-face ((t (:foreground ,forest))))
   `(font-lock-doc-face ((t (:foreground ,green-dark))))
   `(font-lock-function-name-face ((t (:foreground ,purple))))
   `(font-lock-keyword-face ((t (:foreground ,brown :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,red))))
   `(font-lock-preprocessor-face ((t (:foreground ,blue-dark))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,green :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow :weight bold))))
   `(font-lock-string-face ((t (:foreground ,red))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,primary))))
   `(font-lock-warning-face ((t (:foreground ,yellow :weight bold))))

;;;; minibuffer
   `(minibuffer-prompt ((t (:inherit font-lock-keyword-face))))

;;;; link
   `(link ((t (:inherit font-lock-keyword-face :underline t))))

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
   `(ido-incomplete-regex ((t (:inherit error))))
   `(ido-only-match ((t (:inherit success :weight bold))))
   `(ido-subdir ((t (:inherit font-lock-type-face))))

;;;; show-paren
   `(show-paren-match ((t (:inherit font-lock-type-face :weight bold))))
   `(show-paren-mismatch ((t (:inherit error))))

;;;; isearch
   `(isearch ((t (:foreground ,salmon :weight bold))))
   `(isearch-fail ((t (:inherit error))))
   `(lazy-highlight ((t (:inherit isearch))))

;;;; visual-regexp
   `(vr/match-0 ((t (:weight bold :underline t))))
   `(vr/match-1 ((t (:inherit vr/match-0))))
   `(vr/group-0 ((t (:foreground ,yellow))))
   `(vr/group-1 ((t (:foreground ,blue))))
   `(vr/group-2 ((t (:foreground ,green))))

;;;; compilation
   `(compilation-info ((t (:inherit font-lock-constant-face :underline t))))
   `(compilation-mode-line-exit ((t (:inherit font-lock-doc-face :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit error))))
   `(compilation-mode-line-run ((t (:inherit font-lock-keyword-face))))

;;;; company
   `(company-preview ((t (:background ,gray))))
   `(company-preview-common ((t (:inherit company-preview :foreground ,primary))))
   `(company-scrollbar-bg ((t (:inherit company-tooltip))))
   `(company-scrollbar-fg ((t (:inherit company-tooltip-selection))))
   `(company-tooltip ((t (:inherit company-preview))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground ,primary))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection
						    :foreground ,primary))))
   `(company-tooltip-selection ((t (:background ,gray-dark))))

;;;; diff
   `(diff-added ((t (:foreground ,forest :background nil))))
   `(diff-indicator-added ((t :inherit diff-added)))
   `(diff-file-header ((t (:background ,black :weight bold))))
   `(diff-header ((t (:background ,black))))
   `(diff-removed ((t (:foreground ,red :background nil))))
   `(diff-indicator-removed ((t :inherit diff-removed)))
   `(diff-refine-added ((t :inherit default :weight bold
			   :box (:line-width 1 :color ,forest))))
   `(diff-refine-removed ((t :inherit default :weight bold
			     :box (:line-width 1 :color ,red))))

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
   `(cscope-line-number-face ((t (:inherit font-lock-string-face))))
   `(cscope-separator-face ((t (:inherit font-lock-variable-name-face
					 :weight bold))))

;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-builtin-face))))
   `(message-header-bcc ((t (:inherit font-lock-variable-name-face))))
   `(message-header-cc ((t (:inherit font-lock-variable-name-face))))
   `(message-header-from ((t (:inherit font-lock-variable-name-face))))
   `(message-header-name ((t (:inherit font-lock-comment-face :weight bold))))
   `(message-header-other ((t (:inherit font-lock-type-face))))
   `(message-header-subject ((t (:inherit font-lock-builtin-face))))
   `(message-header-to ((t (:inherit font-lock-variable-name-face))))
   `(message-separator ((t (:inherit font-lock-comment))))

;;;;; notmuch
   `(notmuch-tree-match-author-face ((t (:inherit font-lock-variable-name-face))))
   `(notmuch-tree-match-date-face ((t (:inherit font-lock-comment-face))))

;;;; org-mode
   `(org-agenda-date-today ((t (:foreground ,white :slant italic :weight bold))))
   `(org-agenda-structure ((t (:inherit font-lock-comment-face))))
   `(org-date ((t (:foreground ,blue :underline t))))
   `(org-done ((t (:bold t :weight bold :foreground ,green-dark))))
   `(org-level-1 ((t (:foreground ,primary))))
   `(org-level-2 ((t (:foreground ,forest))))
   `(org-level-3 ((t (:foreground ,blue))))
   `(org-level-4 ((t (:foreground ,yellow))))
   `(org-link ((t (:foreground ,yellow :underline t))))
   `(org-table ((t (:foreground ,green-dark))))
   `(org-todo ((t (:inherit font-lock-string-face :weight bold))))

;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:inherit font-lock-type-face :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,foreground))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit font-lock-keyword-face))))
   `(erc-input-face ((t (:inherit font-lock-keyword-face))))
   `(erc-keyword-face ((t (:inherit font-lock-type-face :weight bold))))
   `(erc-my-nick-face ((t (:inherit font-lock-string-face :weight bold))))
   `(erc-nick-default-face ((t (:inherit font-lock-keyword-face))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:inherit font-lock-comment-face))))
   `(erc-pal-face ((t (:inherit font-lock-variable-name-face :weight bold))))
   `(erc-prompt-face ((t (:inherit font-lock-variable-name-face :weight bold))))
   `(erc-timestamp-face ((t (:inherit font-lock-doc-face))))
   ))

;; set ansi color
(theme-colors-with
  (custom-theme-set-variables
   'jm
   `(ansi-color-names-vector [,black
			      ,red
			      ,green
			      ,yellow
			      ,blue
			      "magenta3"
			      "cyan3"
			      ,foreground])))


(provide-theme 'jm)
