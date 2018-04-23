;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                ORG CONFIG                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode
(require 'org)
(setq org-log-done t)
(setq org-agenda-files nil)

;; org todo keywords
(setq org-todo-keywords
       '((sequence "TODO" "WORKING" "UNMERGED" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("WORKING" . "yellow")
	("UNMERGED" . (:foreground "lightblue" :weight bold))))

;; show org-mode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; use current window
(setq org-agenda-window-setup 'current-window)

;; display todo in org agenda
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "")
	  (tags-todo "perso")
	  (tags-todo "work")))))

(defun jm-org-agenda ()
  (interactive)
  (org-agenda nil " "))


(provide 'my-org)
