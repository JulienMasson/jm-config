;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                MPTA TEXT                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; IA FW:   Master=68 Channel=0
;; Chaabi:  Master=70 Channel=0
;; SCU:     Master=69 Channel=24 and Channel=3
;; OS:      Master=73

(defvar mpta-cmd "pti_dumper -m 68,70,73 --lauterbach 7650")
;;(setq mpta-cmd "pti_dumper --lauterbach 7650")
(defvar mpta-buffer-name "*MPTA*")
(defvar mpta-faces '((".*\\(error\\|fail\\).*$"	.	'error)
		     ("\\\[<[0-9a-f]+>\\\]"	.	'warning)))
(defvar mpta-timestamp "%Y/%m/%d  %H:%M:%S  ")
;; (defvar mpta-timestamp nil)

(defun mpta-insert (process text)
  "Insert TEXT in PROCESS buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          ;; If the cursor is at the end, append text like we would be in
          ;; "tail".
          (if (eq (point) (point-max))
              (progn
                (when mpta-timestamp
                  (insert (propertize (format-time-string mpta-timestamp (current-time))
				      'face 'font-lock-comment-face)))
                (insert (concat text "\n"))
                (set-marker (process-mark process) (point)))
            ;; But if not, let the cursor where it is, so `save-excursion'.
            (save-excursion
              (goto-char (point-max))
	      (when mpta-timestamp
		(insert (propertize (format-time-string mpta-timestamp (current-time))
				    'face 'font-lock-comment-face)))
              (insert (concat text "\n"))
              (set-marker (process-mark process) (point)))))))))

(defun mpta-process-filter (process msg)
  "Filter PROCESS output MSG."
  (dolist (msg-line (split-string msg "[\n\r]+"))
    (let ((msg-data (nth 1 (split-string msg-line "|")))
	  (buffer (process-buffer process)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (if msg-data
	      (progn 
		(dolist (f mpta-faces)
		  (if (string-match (car f) msg-data)
		      (setq msg-data (propertize msg-data 'face (cdr f)))))
		(mpta-insert process msg-data))))))))

(defun mpta ()
  "Start MPTA."
  (interactive)
  (let* ((buffer (get-buffer-create mpta-buffer-name)))
    (unless (get-buffer-process buffer)
      (let ((process (start-process-shell-command
                      "mpta"
                      buffer
                      mpta-cmd)))
	(set-process-filter process 'mpta-process-filter))))
  (switch-to-buffer mpta-buffer-name))

;; TODO
;; (set-process-sentinel process 'mpta-process-sentinel)


(provide 'mpta-text)


;; (defun mpta ()
;;   "Start MPTA"
;;   (interactive)
;;   (let ((default-directory pti-dumper-path))
;;     (start-file-process "mpta" mpta-buffer "bash" "-c" mpta-cmd)
;;     (switch-to-buffer mpta-buffer)))
