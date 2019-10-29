;;; jmail-search.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
;; Created: 2019-07-12

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'jmail-view)

;;; Mode

(defvar jmail-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'jmail-search-action-at-point-or-region)
    (define-key map "A" 'jmail-search-action-thread)

    (define-key map "d" 'jmail-search-delete-at-point-or-region)
    (define-key map "D" 'jmail-search-delete-thread)

    (define-key map "m" 'jmail-search-mark-at-point-or-region)
    (define-key map "M" 'jmail-search-mark-thread)

    (define-key map "p" 'jmail-search-apply-patch)
    (define-key map "P" 'jmail-search-apply-patch-series)

    (define-key map "r" 'jmail-search-move-at-point-or-region)
    (define-key map "R" 'jmail-search-move-thread)

    (define-key map "t" 'jmail-search-toggle-thread)
    (define-key map "T" 'jmail-search-toggle-related)

    (define-key map (kbd "TAB") 'jmail-search-fold-unfold-thread)
    (define-key map (kbd "C-TAB") 'jmail-search-fold-unfold-all-thread)

    (define-key map "g" 'jmail-search-refresh)

    (define-key map "L" 'jmail-search-display-all)

    (define-key map "n" 'jmail-search-next)
    (define-key map "p" 'jmail-search-previous)
    (define-key map (kbd "C-<down>") 'jmail-search-next-thread)
    (define-key map (kbd "C-<up>") 'jmail-search-previous-thread)
    (define-key map (kbd "M-<right>") 'jmail-search-next-query)
    (define-key map (kbd "M-<left>") 'jmail-search-previous-query)

    (define-key map [return] 'jmail-search-enter)
    (define-key map (kbd "<C-return>") 'jmail-search-show-this-thread)

    (define-key map "q" 'jmail-search-quit)

    map)
  "Keymap for `jmail-search-mode'")

(define-derived-mode jmail-search-mode fundamental-mode
  "jmail search"
  (jmail-search--insert-header-line)
  (setq-local hl-line-face 'jmail-search-hl-line)
  (setq truncate-lines t)
  (add-hook 'window-scroll-functions #'jmail-search--after-scroll nil t)
  (add-hook 'isearch-mode-hook #'jmail-search-display-all nil t)
  (toggle-read-only t))

;;; Faces

(defface jmail-search-hl-line
  '((t :inherit region :weight bold :underline t))
  "Face with which to highlight the current line in `jmail-search-mode'"
  :group 'jmail)

(defface jmail-search-overlay-fold-face
  '((t :inherit 'font-lock-keyword-face))
  "Default face used to display `jmail-search--overlay-string'"
  :group 'jmail)

(defface jmail-search-results-footer-face
  '((t :slant italic))
  "Default face used to display results footer"
  :group 'jmail)

;;; Customization

(defcustom jmail-search-actions nil
  "Alist of actions to apply at point or region or thread"
  :type 'alist
  :group 'jmail)

(defcustom jmail-search-mark-flags '(("read"      . jmail-search--mark-as-read)
				     ("unread"    . jmail-search--mark-as-unread)
				     ("flagged"   . jmail-search--mark-as-flagged)
				     ("unflagged" . jmail-search--mark-as-unflagged))
  "Alist of flags used to mark messages"
  :type 'alist
  :group 'jmail)

(defcustom jmail-search-fields '("cc:" "bcc:" "from:" "to:" "subject:"
				 "body:" "maildir:" "msgid:" "prio:"
				 "flag:" "date:" "size:" "embed:" "file:"
				 "mime:" "tag:" "list:")
  "List of search fields supported"
  :type 'list
  :group 'jmail)

;;; Internal Variables

(defconst jmail-search--process-buffer-name "*jmail-search-process*")

(defvar jmail-search--process-next nil)

(defconst jmail-search--buffer-name "*jmail-search*")

(defconst jmail-search--format "%s %-11s %-16s  %s")

(defvar-local jmail-search--current nil)

(defvar jmail-search--objects-thread nil)

(defvar-local jmail-search--saved-objects nil)

(defvar-local jmail-search--done nil)

(defvar-local jmail-search--overlays nil)

(defvar-local jmail-search--fold-overlays nil)

(defconst jmail-search--overlay-string " [...]")

(defvar jmail-search--saved nil)

(defvar jmail-search--saved-index 0)

;;; Internal Functions

(defmacro with-jmail-search-buffer (&rest body)
  `(when (get-buffer jmail-search--buffer-name)
     (with-current-buffer jmail-search--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-search--insert-header-line ()
  (with-jmail-search-buffer
   (setq header-line-format (format jmail-search--format " 🔽" "Date" "From" "Subject"))
   (force-mode-line-update)))

(defun jmail-search--subject-str (object)
  (let* ((subject (plist-get object :subject))
	 (thread (plist-get object :thread)))
    (if thread
	(let* ((level (plist-get thread :level))
	       (last-child (plist-get thread :last-child))
	       (spaces (make-string (* level 4) (string-to-char " "))))
	  (cond ((= level 0)
		 subject)
		((= level 1)
		 (if last-child
		     (format "  ┗━▶ %s" subject)
		   (format "  ┣━▶ %s" subject)))
		((> level 1)
		 (if last-child
		     (format "  ┃%s┗━▶ %s" spaces subject)
		   (format "  ┃%s┣━▶ %s" spaces subject)))))
      subject)))

(defun jmail-search--date-str (object)
  (propertize (format-time-string "%F" (plist-get object :date))
	      'face 'font-lock-comment-face))

(defun jmail-search--from-str (object)
  (if-let ((from (car (plist-get object :from))))
      (if-let ((name (car from)))
	  (propertize (truncate-string-to-width name 16)
		      'face 'font-lock-variable-name-face)
	(propertize (truncate-string-to-width (cdr from) 16)
		    'face 'font-lock-variable-name-face))
    (propertize "unknown" 'face 'font-lock-variable-name-face)))

(defun jmail-search--parse-object (object)
  (list (jmail-search--date-str object)
	(jmail-search--from-str object)
	(jmail-search--subject-str object)))

(defun jmail-search--insert-overlay (start end str)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string str)
    (add-to-list 'jmail-search--overlays overlay)))

(defun jmail-search--find-overlay (pos)
  (cl-find-if (lambda (ov)
	      (and (<= (overlay-start ov) pos)
		   (>= (overlay-end ov) pos)))
	    jmail-search--overlays))

(defun jmail-search--delete-all-overlays ()
  (with-jmail-search-buffer
   (delete-all-overlays)
   (setq jmail-search--fold-overlays nil)
   (setq jmail-search--overlays nil)))

(defun jmail-search--insert-flag (start object)
  (let ((flags (plist-get object :flags))
	(unread (propertize "U " 'face 'error))
	(flagged (propertize "S " 'face 'font-lock-warning-face)))
    (cond ((member 'unread flags)
	   (jmail-search--insert-overlay start (+ start 2) unread))
	  ((member 'flagged flags)
	   (jmail-search--insert-overlay start (+ start 2) flagged))
	  (t (jmail-search--insert-overlay start (+ start 2) "  ")))))

(defun jmail-search--process-results (object)
  (with-jmail-search-buffer
   (cl-multiple-value-bind (date from subject)
       (jmail-search--parse-object object)
     (save-excursion
       (goto-char (point-max))
       (insert (format jmail-search--format " " date from subject))
       (add-text-properties (line-beginning-position) (line-end-position) object)
       (jmail-search--insert-flag (line-beginning-position) object)
       (insert "\n")))))

(defun jmail-search--remove-flag (pos)
  (when-let ((overlay (jmail-search--find-overlay pos)))
    (overlay-put overlay 'before-string "  ")))

(defun jmail-search--set-flag (pos value)
  (when-let ((overlay (jmail-search--find-overlay pos)))
    (overlay-put overlay 'before-string value)))

(defun jmail-search--set-property (prop value)
  (put-text-property (line-beginning-position)
		     (line-end-position)
		     prop value))

(defun jmail-search--funcall-object-at (func)
  (when-let ((object (text-properties-at (point))))
    (funcall func object)))

(defun jmail-search--mark-as-read ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (new-path path))
     (when (member 'new flags)
       (setq flags (remove 'new flags))
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string "new" "cur" new-path)))
     (when (member 'unread flags)
       (jmail-search--remove-flag point)
       (setq flags (remove 'unread flags))
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string ",$" ",S" new-path)))
     (unless (string= path new-path)
       (jmail-search--set-property :path new-path)
       (rename-file path new-path)))))

(defun jmail-search--mark-as-unread ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (str (propertize "U " 'face 'error))
	       (new-path path))
     (unless (member 'unread flags)
       (jmail-search--set-flag point str)
       (push 'unread flags)
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string ",\\([A-Z]*\\)S$"
						",\\1"
						new-path))
       (jmail-search--set-property :path new-path)
       (rename-file path new-path)))))

(defun jmail-search--mark-as-flagged ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (str (propertize "S " 'face 'font-lock-warning-face))
	       (new-path path))
     (unless (member 'flagged flags)
       (jmail-search--set-flag point str)
       (push 'flagged flags)
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string ",\\([A-Z]*\\)"
						",F\\1" new-path))
       (jmail-search--set-property :path new-path)
       (rename-file path new-path)))))

(defun jmail-search--mark-as-unflagged ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (new-path path))
     (when (member 'flagged flags)
       (jmail-search--remove-flag point)
       (setq flags (remove 'flagged flags))
       (jmail-search--set-property :flags flags)
       (setq new-path (replace-regexp-in-string ",F\\([A-Z]*\\)$"
						",\\1" new-path))
       (jmail-search--set-property :path new-path)
       (rename-file path new-path)))))

(defun jmail-search--args (query thread related)
  (let ((option "find")
	(default-args (list "--reverse" "--format=sexp" query)))
    (when thread
      (push "--threads" default-args))
    (when related
      (push "--include-related" default-args))
    (push option default-args)))

(defun jmail-search--insert-footer ()
  (with-jmail-search-buffer
   (when (and jmail-search--done
	      (not jmail-search--saved-objects))
     (save-excursion
       (goto-char (point-max))
       (if (and (eq (point-min) (point-max))
		(not (eq (cadr (text-properties-at (point-min)))
			 'jmail-search-results-footer-face)))
	   (insert (propertize "No results found !!!!"
			       'face 'jmail-search-results-footer-face))
	 (unless (eq (cadr (text-properties-at (- (point-max) 1)))
		     'jmail-search-results-footer-face)
	   (insert (propertize "End of search results."
			       'face 'jmail-search-results-footer-face))))))))

(defun jmail-search--after-scroll (win _start)
  (jmail-search--display-saved-objects))

(defun jmail-search--save-objects-handler ()
  (with-jmail-search-buffer
   (let (object)
     (while (setq object (jmail-extract-sexp-object
			  jmail-search--process-buffer-name))
       (add-to-list 'jmail-search--saved-objects object t))))
  (when jmail-search--done
    (kill-buffer jmail-search--process-buffer-name)))

(defun jmail-search--save-objects ()
  (unless (and jmail-search--objects-thread
	       (thread-live-p jmail-search--objects-thread))
    (setq jmail-search--objects-thread
	  (make-thread #'jmail-search--save-objects-handler))))

(defun jmail-search--display-saved-objects ()
  (with-jmail-search-buffer
   (while (and jmail-search--saved-objects
	       (< (count-lines (window-start) (point-max))
		  (window-total-height)))
     (jmail-search--process-results (pop jmail-search--saved-objects))))
  (jmail-search--insert-footer))

(defun jmail-search--display-objects (buffer)
  (let (object)
    (while (setq object (jmail-extract-sexp-object buffer))
      (jmail-search--process-results object))))

(defun jmail-search--process-objects (buffer)
  (with-jmail-search-buffer
   (if (>= (count-lines (window-start (get-buffer-window (current-buffer)))
		       (point-max))
	   (window-total-height))
       (jmail-search--save-objects)
     (jmail-search--display-objects buffer)
     (when (and jmail-search--done
		(or (not jmail-search--objects-thread)
		    (not (thread-live-p jmail-search--objects-thread))))
       (kill-buffer jmail-search--process-buffer-name)))))

(defun jmail-search--process-done ()
  (with-jmail-search-buffer
   (setq jmail-search--done t)))

(defun jmail-search--process-sentinel (process status)
  (jmail-search--process-done)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (jmail-search--process-objects (process-buffer process)))
  (jmail-search--insert-footer)
  (when jmail-search--process-next
    (jmail-search--setup-env)
    (jmail-search--process-run jmail-search--process-next)
    (setq jmail-search--process-next nil)))

(defun jmail-search--process-filter (process str)
  (unless (eq (process-status process) 'signal)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert str)
      (jmail-search--process-objects (current-buffer)))))

(defun jmail-search--process-run (args)
  (when-let* ((default-directory jmail-top-maildir)
	      (program (jmail-find-program jmail-index-program))
	      (buffer (get-buffer-create jmail-search--process-buffer-name))
	      (process (apply 'start-file-process "jmail-search-process" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-search--process-filter)
    (set-process-sentinel process 'jmail-search--process-sentinel)))

(defun jmail-search--stop-process ()
  (setq jmail-search--process-next nil)
  (jmail-terminate-process-buffer jmail-search--process-buffer-name))

(defun jmail-search--process (args)
  (if (jmail-terminate-process-buffer jmail-search--process-buffer-name)
      (setq jmail-search--process-next args)
    (jmail-search--process-run args)))

(defun jmail-search--setup-env ()
  (with-jmail-search-buffer
   (jmail-search--delete-all-overlays)
   (erase-buffer)
   (setq jmail-search--done nil)
   (setq jmail-search--saved-objects nil)))

(defun jmail-search--run (query thread related &optional save)
  (unless (get-buffer jmail-search--buffer-name)
    (with-current-buffer (get-buffer-create jmail-search--buffer-name)
      (jmail-search-mode)))
  (with-jmail-search-buffer
   (jmail-search--setup-env)
   (setq jmail-search--current `(:query ,query
				 :thread ,thread
				 :related ,related))
   (when save
     (push jmail-search--current jmail-search--saved))
   (switch-to-buffer (current-buffer))
   (jmail-search--process (jmail-search--args query thread related))))

(defun jmail-search--thread-level-at-point ()
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (thread (plist-get object :thread)))
       (plist-get thread :level))))

(defun jmail-search--goto-root-thread ()
  (when-let* ((object (text-properties-at (point)))
	      (thread (plist-get object :thread))
	      (level (plist-get thread :level)))
    (unless (zerop level)
      (previous-line)
      (jmail-search--goto-root-thread))))

(defmacro jmail-search--foreach-line-thread (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (jmail-search--goto-root-thread)
      ,@body
      (next-line)
      (while (and (jmail-search--thread-level-at-point)
		  (not (zerop (jmail-search--thread-level-at-point))))
	,@body
	(next-line)))))

(defmacro jmail-search--foreach-line-region (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (when (region-active-p)
	(lexical-let ((beg (region-beginning))
		      (end (region-end)))
	  (deactivate-mark)
	  (goto-char beg)
	  (while (<= (point) end)
	    ,@body
	    (next-line)))))))

(defun jmail-search--thread-range ()
  (if-let ((level (jmail-search--thread-level-at-point)))
      (let ((start (line-end-position))
	    end)
	(save-excursion
	  (forward-line)
	  (while (and (jmail-search--thread-level-at-point)
		      (< level (jmail-search--thread-level-at-point))
		      (not (eobp)))
	    (forward-line))
	  (setq end (- (point) 1)))
	(list start end))
    (list (point) (point))))

(defun jmail-search--find-fold-overlay (start end)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) start)
		     (>= (overlay-end ov) end)))
	      jmail-search--fold-overlays))

(defun jmail-search--remove-fold-overlay (overlay)
  (setq jmail-search--fold-overlays (remove overlay jmail-search--fold-overlays))
  (delete-overlay overlay))

(defun jmail-search--add-fold-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (add-to-list 'jmail-search--fold-overlays overlay)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string
		 (propertize jmail-search--overlay-string
			     'face 'jmail-search-overlay-fold-face))))

(defun jmail-search--remove-current-line ()
  (save-excursion
    (beginning-of-line)
    (when-let ((overlay (jmail-search--find-overlay (point))))
      (setq jmail-search--overlays (remove overlay jmail-search--overlays))
      (delete-overlay overlay))
    (delete-region (point) (+ (line-end-position) 1))))

(defun jmail-search--delete-message ()
  (interactive)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path)))
     (delete-file path)
     (jmail-search--remove-current-line))))

(defun jmail-search--read-uidvalidity (dir)
  (when-let* ((default-directory dir)
	      (data (with-temp-buffer
		      (insert-file-contents ".uidvalidity")
		      (split-string (buffer-string) "\n" t))))
    (string-to-number (cadr data))))

(defun jmail-search--write-uidvalidity (dir val)
  (let ((default-directory dir)
	(message-log-max nil))
    (with-current-buffer (find-file-noselect ".uidvalidity")
      (goto-char (- (point-max) 1))
      (delete-region (line-beginning-position)
		     (line-end-position))
      (insert (number-to-string val))
      (save-buffer)
      (kill-buffer))))

(defun jmail-search--move-message (dest-dir)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path))
	       (src-dir (expand-file-name (concat path "/../../")))
	       (dest-uid (jmail-search--read-uidvalidity dest-dir))
	       (new-path (replace-regexp-in-string
			  ".*/\\(cur\\|new\\|tmp\\)\\(.*U=\\)[0-9]+\\(:.*\\)"
			  (format "%s/\\1\\2%d\\3" dest-dir (+ dest-uid 1))
			  path)))
     (jmail-search--set-property :path new-path)
     (jmail-search--write-uidvalidity dest-dir (+ dest-uid 1))
     (rename-file path new-path))))

;;; External Functions

(defun jmail-search-action-at-point-or-region (action)
  (interactive (list (completing-read (if (region-active-p)
					  "Apply action on region: "
					"Apply action at point: ")
				      'jmail-search-actions)))
  (if (region-active-p)
      (jmail-search--foreach-line-region
       (jmail-search--funcall-object-at
	(assoc-default action jmail-search-actions)))
    (jmail-search--funcall-object-at
     (assoc-default action jmail-search-actions))))

(defun jmail-search-action-thread (action)
  (interactive (list (completing-read "Apply action on thread: "
				      'jmail-search-actions)))
  (jmail-search--foreach-line-thread
   (jmail-search--funcall-object-at
    (assoc-default action jmail-search-actions))))

(defun jmail-search-delete-at-point-or-region (confirm)
  (interactive (list (yes-or-no-p (if (region-active-p)
				      "Delete all messages from region: "
				    "Delete message: "))))
  (when confirm
    (if (region-active-p)
	(jmail-search--foreach-line-region
	 (jmail-search--delete-message))
      (jmail-search--delete-message))
    (jmail-update-buffer)))

(defun jmail-search-delete-thread (confirm)
  (interactive (list (yes-or-no-p "Delete whole thread: ")))
  (when confirm
    (jmail-search--foreach-line-thread
     (jmail-search--delete-message))
    (jmail-update-buffer)))

(defun jmail-search-mark-at-point-or-region (flag)
  (interactive (list (completing-read (if (region-active-p)
					  "Mark on region as: "
					"Mark at point as: ")
				      (mapcar #'car jmail-search-mark-flags))))
  (if (region-active-p)
      (jmail-search--foreach-line-region
       (funcall (assoc-default flag jmail-search-mark-flags)))
    (funcall (assoc-default flag jmail-search-mark-flags)))
  (jmail-update-buffer))

(defun jmail-search-mark-thread (flag)
  (interactive (list (completing-read "Mark whole thread as: "
				      (mapcar #'car jmail-search-mark-flags))))
  (jmail-search--foreach-line-thread
   (funcall (assoc-default flag jmail-search-mark-flags)))
  (jmail-update-buffer))

(defun jmail-search-apply-patch-series (dir)
  (interactive "DApply patch series: ")
  (jmail-search--foreach-line-thread
      (when-let* ((object (text-properties-at (point)))
		  (thread (plist-get object :thread))
		  (level (plist-get thread :level))
		  (msg (plist-get object :path))
		  (subject (plist-get object :subject))
		  (default-directory dir)
		  (tmp-patch (concat default-directory ".jmail.patch")))
	(when (and (string-match "^\\[PATCH " subject)
		   (= level 1))
	  (if (not (tramp-tramp-file-p default-directory))
	      (shell-command (concat "git am " msg))
	    (copy-file msg tmp-patch t)
	    (shell-command (concat "git am " (jmail-untramp-path tmp-patch)))
	    (delete-file tmp-patch))))))

(defun jmail-search-apply-patch (dir)
  (interactive "DApply patch: ")
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (msg (plist-get object :path))
	       (subject (plist-get object :subject))
	       (default-directory dir)
	       (tmp-patch (concat default-directory ".jmail.patch")))
     (when (string-match "^\\[PATCH " subject)
       (if (not (tramp-tramp-file-p default-directory))
	   (shell-command (concat "git am " msg))
	 (copy-file msg tmp-patch t)
	 (shell-command (concat "git am " (jmail-untramp-path tmp-patch)))
	 (delete-file tmp-patch))))))

(defun jmail-search-move-at-point-or-region (maildir)
  (interactive (list (completing-read (if (region-active-p)
					  "Move region to: "
					"Move to: ")
				      (jmail-maildirs (jmail-get-top-maildir)))))
  (if (region-active-p)
      (jmail-search--foreach-line-region
       (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
    (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
  (jmail-update-buffer))

(defun jmail-search-move-thread (maildir)
  (interactive (list (completing-read "Move whole thread to: "
				      (jmail-maildirs (jmail-get-top-maildir)))))
  (jmail-search--foreach-line-thread
   (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
  (jmail-update-buffer))

(defun jmail-search-toggle-thread ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (not (plist-get jmail-search--current :thread)))
	  (related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread related))))

(defun jmail-search-toggle-related ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (not (plist-get jmail-search--current :related))))
    (jmail-search--run query thread related))))

(defun jmail-search-fold-unfold-thread ()
  (interactive)
  (cl-multiple-value-bind (start end)
      (jmail-search--thread-range)
    (unless (= start end)
      (if-let ((overlay (jmail-search--find-fold-overlay start end)))
	  (jmail-search--remove-fold-overlay overlay)
	(jmail-search--add-fold-overlay start end)))))

(defun jmail-search-fold-unfold-all-thread ()
  (interactive)
  (if jmail-search--fold-overlays
      (mapc #'jmail-search--remove-fold-overlay
	    jmail-search--fold-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(jmail-search-toggle-folding-thread)
	(line-move 1)))))

(defun jmail-search-refresh ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related))))

(defun jmail-search-display-all ()
  (interactive)
  (with-jmail-search-buffer
   (when jmail-search--saved-objects
     (let ((total (length jmail-search--saved-objects)))
       (while jmail-search--saved-objects
	 (jmail-search--process-results (pop jmail-search--saved-objects))
	 (message "Display all results: %d%%"
		  (/ (* (- total (length jmail-search--saved-objects)) 100)
		     total))))
     (jmail-search--insert-footer))))

(defun jmail-search-next ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer jmail-search--buffer-name))
    (switch-to-buffer-other-window jmail-search--buffer-name))
  (with-jmail-search-buffer
   (next-line)
   (jmail-search-enter)))

(defun jmail-search-previous ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer jmail-search--buffer-name))
    (switch-to-buffer-other-window jmail-search--buffer-name))
  (with-jmail-search-buffer
   (previous-line)
   (jmail-search-enter)))

(defun jmail-search-next-thread ()
  (interactive)
  (next-line)
  (when-let ((level (jmail-search--thread-level-at-point)))
    (while (and level (not (zerop level)))
      (next-line)
      (setq level (jmail-search--thread-level-at-point)))))

(defun jmail-search-previous-thread ()
  (interactive)
  (previous-line)
  (jmail-search--goto-root-thread))

(defun jmail-search-next-query ()
  (interactive)
  (let* ((size (length jmail-search--saved))
	 (index (if (zerop jmail-search--saved-index)
		    (- size 1)
		  (- jmail-search--saved-index 1)))
	 (search (nth index jmail-search--saved)))
    (setq jmail-search--current search)
    (setq jmail-search--saved-index index)
    (jmail-search-refresh)))

(defun jmail-search-previous-query ()
  (interactive)
  (let* ((size (length jmail-search--saved))
	 (index (mod (+ jmail-search--saved-index 1) size))
	 (search (nth index jmail-search--saved)))
    (setq jmail-search--current search)
    (setq jmail-search--saved-index index)
    (jmail-search-refresh)))

(defun jmail-search-enter ()
  (interactive)
  (with-jmail-search-buffer
   (jmail-search--mark-as-read)
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path)))
     (jmail-view path (current-buffer)))))

(defun jmail-search-show-this-thread ()
  (interactive)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (message-id (plist-get object :message-id))
	       (query (concat "msgid:" message-id)))
     (setq jmail-search--saved-index 0)
     (jmail-search--run query t t t))))

(defun jmail-search-quit ()
  (interactive)
  (jmail-search--stop-process)
  (jmail-view-quit)
  (jmail-search--delete-all-overlays)
  (with-jmail-search-buffer
   (kill-buffer))
  (jmail))

(defun jmail-search (query)
  (setq jmail-search--saved-index 0)
  (jmail-search--run query t nil t))

(provide 'jmail-search)
