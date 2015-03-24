;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ORG CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(setq org-log-done t)


;; org todo keywords
(setq org-todo-keywords
       '((sequence "TODO" "WORKING" "UNMERGED" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WORKING" . "yellow")
	("UNMERGED" . (:foreground "lightblue" :weight bold))))


;; add org agenda
(setq org-agenda-files (list "~/org/notes.org"
			     "~/org/agenda.org"
			     "~/org/todo.org"))

;; use current window
(setq org-agenda-window-setup 'current-window)

;; org capture wl
(require 'org-wl)

;; org capture for wanderlust
(setq org-capture-templates '(("n" "Notes" entry
			       (file+headline "~/org/notes.org" "Extra")
			       " TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
			      ("l" "Link" plain
			       (file "~/org/notes.org" "Links")
			       "- %?\n %x\n")
			      ("t" "Todo" entry
			       (file+headline "~/org/todo.org" "Tasks")
			       " TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
			      ("j" "Journal" entry
			       (file+datetree "~/org/journal.org")
			       "* %?\nEntered on %U\n  %i\n  %a")
			      ("e" "Email Todo" entry
			       (file+headline "~/org/todo.org" "Mails")
			       "* %^{Brief Description}\nEmail: %a\nFrom: %:from \nTo: %:to \n%?Added: %U\n" :prepend t)
			      ))

(provide 'my-org)
