;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                RSS CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set default web browser
(setq browse-url-browser-function 'browse-url-chromium
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;; rss readers
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/"))
;;(require 'init-newsticker)

(require 'newsticker)

(setq
 newsticker-heading-format "%t"
 newsticker-item-format "%t"
 newsticker-desc-format "%d\n%c"
 newsticker-hide-old-items-in-newsticker-buffer t

 newsticker-html-renderer 'w3m-region

 newsticker-frontend 'newsticker-treeview
 newsticker-use-full-width nil

 newsticker-retrieval-interval 0   ;don't fetch when I'm not reading RSS
 newsticker-automatically-mark-items-as-old nil
 newsticker-url-list '(
		       ("Les crises" "http://feeds.feedburner.com/les-crises-fr" nil nil nil)
		       ("news360x" "http://www.news360x.fr/feed/" nil nil nil)
		       ("RT" "http://rt.com/rss/" nil nil nil)
		       ("Embedded" "http://www.embedded.com/rss/all" nil nil nil)
		       ("Linux" "http://www.linux.com/feeds/all-content" nil nil nil)
		       ("Econoclastes" "http://leseconoclastes.fr/feed/" nil nil nil)
		       ("Fubiz" "http://feeds.feedburner.com/fubiz" nil nil nil)))

(provide 'my-rss)
