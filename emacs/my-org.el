;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ORG CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
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
			      ("j" "Journal" entry
			       (file+headline "~/org/journal.org" "")
			       "\n %^{topic} %T \n%i%?\n" :prepend t)
			      ("e" "Email Todo" entry
			       (file+headline "~/org/todo.org" "Mails")
			       "* TODO %^{Brief Description}\nEmail: %a\nFrom: %:from \nTo: %:to \n%?Added: %U\n" :prepend t)
			      ))

(provide 'my-org)
