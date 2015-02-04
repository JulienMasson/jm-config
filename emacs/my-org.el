;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ORG CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(setq org-log-done t)

;; add org agenda
(setq org-agenda-files (list "~/org/notes.org"
			     "~/org/journal.org"
			     "~/org/todo.org"))

;; org capture
(require 'org-wl)

;; org capture for wanderlust
(setq org-capture-templates '(("n" "Notes" entry
			       (file+headline "~/org/notes.org" "Notes")
			       " TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
			      ("t" "Todo" entry
			       (file+headline "~/org/todo.org" "Tasks")
			       " TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
			      ("l" "Link" plain
			       (file "~/org/notes.org" "Links")
			       "- %?\n %x\n")
			      ("t" "Todo" entry
			       (file+headline "~/org/todo.org" "Tasks")
			       "* TODO %?\n %i\n")
			      ("j" "Journal" entry
			       (file+datetree "~/org/journal.org")
			       "* %?\nEntered on %U\n  %i\n  %a")
			      ("e" "Email Todo" entry
			       (file+headline "~/org/todo.org" "Mails")
			       "* TODO %^{Brief Description}\nEmail: %a\nFrom: %:from \nTo: %:to \n%?Added: %U\n" :prepend t)
			      ))

(provide 'my-org)
