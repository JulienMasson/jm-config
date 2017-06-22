;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                MPTA TEXT                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global vars
(defvar mpta-cmd "pti_dumper --lauterbach 7650")
(defvar mpta-buffer-name "*MPTA*")
(defvar mpta-faces '((".*\\(error\\|fail\\).*$"	.	'error)
		     ("\\\[<[0-9a-f]+>\\\]"	.	'warning)))
(defvar mpta-timestamp "%H:%M:%S ")

(defgroup mpta nil
  "mpta group."
  :group 'convenience)

(define-derived-mode mpta-mode fundamental-mode
  "mpta"
  (local-set-key (kbd "q") 'mpta-quit)
  (local-set-key (kbd "i") 'mpta-toggle-iafw)
  (local-set-key (kbd "c") 'mpta-toggle-chaabi)
  (local-set-key (kbd "s") 'mpta-toggle-scu)
  (local-set-key (kbd "o") 'mpta-toggle-os)
  (local-set-key (kbd "a") 'mpta-enable-all)
  (local-set-key (kbd "d") 'mpta-disable-all)
  (local-set-key (kbd "d") 'mpta-delete-log-buffer)
  (toggle-read-only t))

(defvar mpta-enable-mode-line-p '(member
				  major-mode
				  '(mpta-mode)))

(defvar mpta-mode-line-string nil)
(put 'mpta-mode-line-string 'risky-local-variable t)

;; iafw vars
(defvar iafw-match "Master= 68")
(defvar iafw-toggle t)
(defface iafw-face
  '((t (:foreground "saddle brown")))
  "face for iafw"
  :group 'mpta)

;; chaabi vars
(defvar chaabi-match "Master= 70")
(defvar chaabi-toggle t)
(defface chaabi-face '((t (:foreground "orange")))
  "face for chaabi"
  :group 'mpta)

;; scu vars
(defvar scu-match "Master= 69 Channel=  \\(?:3\\|7\\|24\\) ")
(defvar scu-toggle t)
(defface scu-face '((t (:foreground "dim gray")))
  "face for scu"
  :group 'mpta)

;; os vars
(defvar os-match "Master= 73")
(defvar os-toggle t)
(defface os-face '((t (:foreground "indian red")))
  "face for os"
  :group 'mpta)

;; functions
(defun mpta-insert (process timestamp title data)
  "Insert TEXT in PROCESS buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
	  (light-save-excursion-if-not-at-point-max buffer
	    (goto-char (point-max))
	    (insert (concat timestamp title data "\n"))))))))

(defun mpta-process-filter (process msg)
  "Filter PROCESS output MSG."
  (dolist (msg-line (split-string msg "[\n\r]+"))
    (let (title data
	  (timestamp (propertize (format-time-string mpta-timestamp (current-time))
				 'face 'font-lock-comment-face)))
      (cond
       ;; iafw
       ((and (string-match iafw-match msg-line) iafw-toggle)
       	(setq title (propertize (format "%-8s" "IAFW:") 'face 'iafw-face))
       	(setq data (replace-regexp-in-string (concat ".*" iafw-match ".*|\\(.*\\)|") "\\1" msg-line)))
       ;; chaabi
       ((and (string-match chaabi-match msg-line) chaabi-toggle)
       	(setq title (propertize (format "%-8s" "CHAABI:") 'face 'chaabi-face))
       	(setq data (replace-regexp-in-string (concat ".*" chaabi-match ".*|\\(.*\\)|") "\\1" msg-line)))
       ;; scu
       ((and (string-match scu-match msg-line) scu-toggle)
       	(setq title (propertize (format "%-8s" "SCU:") 'face 'scu-face))
       	(setq data (replace-regexp-in-string (concat ".*" scu-match ".*|\\(.*\\)|") "\\1" msg-line)))
       ;; os
       ((and (string-match os-match msg-line) os-toggle)
       	(setq title (propertize (format "%-8s" "OS:") 'face 'os-face))
       	(setq data (replace-regexp-in-string (concat ".*" os-match ".*|\\(.*\\)|") "\\1" msg-line))))
      ;; insert text
      (when (> (length data) 0)
	(dolist (f mpta-faces)
	  (if (string-match (car f) data)
	      (setq data (propertize data 'face (cdr f)))))
	(mpta-insert process timestamp title data)))))

(defun mpta-quit ()
  (interactive)
  (when (or (get-buffer-process (current-buffer))
	    (yes-or-no-p "Are you sure you want to kill mpta buffer ?"))
    (kill-buffer)))

(defun mpta-update-mode-line ()
  (let ((mpta-string '()))
    ;; iafw
    (if iafw-toggle
	(add-to-list 'mpta-string (propertize "[IAFW]" 'face 'iafw-face)))
    ;; chaabi
    (if chaabi-toggle
	(add-to-list 'mpta-string (propertize "[CHAABI]" 'face 'chaabi-face)))
    ;; scu
    (if scu-toggle
	(add-to-list 'mpta-string (propertize "[SCU]" 'face 'scu-face)))
    ;; os
    (if os-toggle
	(add-to-list 'mpta-string (propertize "[OS]" 'face 'os-face)))
    (setq mpta-mode-line-string (mapconcat 'identity mpta-string " "))
    (force-mode-line-update)))

(defun mpta-mode-line ()
  "Return a string to display in mode line."
  (when (eval mpta-enable-mode-line-p)
    mpta-mode-line-string))

(defun mpta-delete-log-buffer ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to erase this log buffer ?")
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(defun mpta-toggle-iafw ()
  (interactive)
  (setq iafw-toggle (not iafw-toggle))
  (mpta-update-mode-line))

(defun mpta-toggle-chaabi ()
  (interactive)
  (setq chaabi-toggle (not chaabi-toggle))
  (mpta-update-mode-line))

(defun mpta-toggle-scu ()
  (interactive)
  (setq scu-toggle (not scu-toggle))
  (mpta-update-mode-line))

(defun mpta-toggle-os ()
  (interactive)
  (setq os-toggle (not os-toggle))
  (mpta-update-mode-line))

(defun mpta-enable-all ()
  (interactive)
  (setq iafw-toggle t)
  (setq chaabi-toggle t)
  (setq scu-toggle t)
  (setq os-toggle t)
  (mpta-update-mode-line))

(defun mpta-disable-all ()
  (interactive)
  (setq iafw-toggle nil)
  (setq chaabi-toggle nil)
  (setq scu-toggle nil)
  (setq os-toggle nil)
  (mpta-update-mode-line))

(defun mpta-enable-all ()
  (interactive)
  (setq iafw-toggle t)
  (setq chaabi-toggle t)
  (setq scu-toggle t)
  (setq os-toggle t)
  (mpta-update-mode-line))

(defun mpta ()
  "Start MPTA."
  (interactive)
  (if (executable-find "pti_dumper")
    (with-current-buffer (get-buffer-create mpta-buffer-name)
      (mpta-mode)
      (let ((process (start-process-shell-command
		      "mpta"
		      (current-buffer)
		      mpta-cmd)))
	(set-process-filter process 'mpta-process-filter))
      (pop-to-buffer-same-window (current-buffer))
      (add-to-list 'global-mode-string '(:eval (mpta-mode-line)) t)
      (mpta-enable-all))
    (message (propertize "pti_dumper not found" 'face 'error))))


(provide 'mpta-text)
