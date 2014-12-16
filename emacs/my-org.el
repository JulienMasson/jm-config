;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ORG CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(setq org-log-done t)

;; add org agenda
(setq org-agenda-files (list "~/org/intel.org"
			     "~/org/notes.org"
			     "~/org/journal.org"
			     "~/org/todo.org"
			     "~/org/home.org"))

;; org capture for wanderlust
(setq org-capture-templates '(("t" "Todo" entry
			       (file+headline "~/org/todo.org" "Tasks")
			       " TODO %^{Brief Description} %^g\n%?\nAdded: %U" :prepend t)
			      ("j" "Journal" entry
			       (file+headline "~/org/journal.org" "")
			       "\n %^{topic} %T \n%i%?\n" :prepend t)
			      ("e" "Email Todo" entry
			       (file+headline "~/org/todo.org" "Tasks")
			       "* TODO %^{Brief Description}\n%a\n%?Added: %U\n" :prepend t)
			      ))

(provide 'my-org)
