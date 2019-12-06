;;; company-async-semantic.el --- Custom company-mode completion backend

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

(require 'async-semantic)
(require 'semantic/bovine/c)

;;; Customization

(defcustom company-async-semantic-modes '(c-mode c++-mode)
  "List of mode when `company-async-semantic' is enabled"
  :type 'list
  :group 'company)

(defcustom company-async-semantic-enabled t
  "Enable/Disable `company-async-semantic', by default it's enabled"
  :type 'boolean
  :group 'company)

(defcustom company-async-semantic-includes nil
  "Assoc list with CONS (path . includes) used to set includes path"
  :type 'alist
  :group 'company)

;;; Internal Variables

(defconst company-async-semantic--member-regexp "->\\|\\.\\|::")

(defconst company-async-semantic--include-regexp "#include\\s-+[\"<]\\([a-zA-Z0-9_/]*\\)$")

(defvar company-async-semantic--cache nil)

(defvar company-async-semantic--updating-cache nil)

(defvar-local company-async-semantic--default-path nil)

(defvar-local company-async-semantic--files-dep nil)

;;; Internal Functions

(defun company-async-semantic--set-status (msg)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (memq major-mode company-async-semantic-modes)
	(if (string-match "\\(.*\\):.*" mode-name)
	    (let ((header (match-string 1 mode-name)))
	      (setq mode-name (if msg
				  (format "%s:  %s" header msg)
				header)))
	  (when msg
	    (setq mode-name (format "%s:  %s" mode-name msg))))
	(force-mode-line-update)))))

(defun company-async-semantic--remote-host (file)
  (replace-regexp-in-string "\\(^/ssh:.*:\\).*" "\\1" file))

(defun company-async-semantic--set-default-path ()
  (if-let* ((local-dir (expand-file-name default-directory))
	    (match (seq-find (lambda (dir)
			       (string-prefix-p dir local-dir))
			     (mapcar #'car company-async-semantic-includes))))
      (setq company-async-semantic--default-path
	    (assoc-default match company-async-semantic-includes))
    (if (tramp-tramp-file-p local-dir)
	(setq company-async-semantic--default-path
	      (let ((host (company-async-semantic--remote-host local-dir)))
		(mapcar (lambda (include)
			  (concat host include))
			async-semantic-default-path)))
      (setq company-async-semantic--default-path async-semantic-default-path))))

(defun company-async-semantic--remote-locate-file (file paths)
  (let (match)
    (catch 'match
      (dolist (path paths)
	(when (file-exists-p path)
	  (let* ((default-directory path)
		 (host (company-async-semantic--remote-host path))
		 (local-dir (replace-regexp-in-string host "" path))
		 (program (executable-find "find")))
	  (with-temp-buffer
	    (process-file program nil (current-buffer) nil
			  local-dir "-name" file)
	    (goto-char (point-min))
	    (unless (= (point-min) (point-max))
	      (setq match (concat host (buffer-substring-no-properties
					(line-beginning-position)
					(line-end-position)))))))))
      match)))

(defun company-async-semantic--find-dep (include)
  (when-let ((default-dir (expand-file-name default-directory))
	     (paths (append (list default-dir) company-async-semantic--default-path))
	     (file (semantic-tag-name include)))
    (if (tramp-tramp-file-p default-dir)
	(company-async-semantic--remote-locate-file file paths)
      (locate-file file paths))))

(defun company-async-semantic--get-includes-files-dep (file)
  (when-let* ((tags (assoc-default file company-async-semantic--cache))
	      (includes (seq-filter (lambda (tag)
				      (eq (semantic-tag-class tag) 'include))
				    tags)))
    (mapc (lambda (include)
	    (unless (member include company-async-semantic--files-dep)
	      (add-to-list 'company-async-semantic--files-dep include t)
	      (company-async-semantic--get-includes-files-dep include)))
	  (delq nil (mapcar #'company-async-semantic--find-dep includes)))))

(defun company-async-semantic--get-files-dep ()
  (let ((file (file-truename (buffer-file-name))))
    (setq company-async-semantic--files-dep (list file))
    (company-async-semantic--get-includes-files-dep file)))

(defun company-async-semantic--match (tag prefix compare)
  (let ((name (semantic-tag-name tag))
	(class (semantic-tag-class tag))
	(attr (semantic-tag-attributes tag)))
    (when (or (eq class 'function)
	      (eq class 'variable)
	      (and (eq class 'type)
		   (plist-member attr :members)))
      (funcall compare prefix name))))

(defmacro foreach-company-async-semantic--tag (&rest body)
  (declare (indent 0))
  `(dolist (file company-async-semantic--files-dep)
     (when-let ((tags (assoc-default file company-async-semantic--cache)))
       (dolist (tag tags)
	 ,@body))))

(defun company-async-semantic--completions-system (prefix)
  (let (matchs)
    (foreach-company-async-semantic--tag
      (when (company-async-semantic--match tag prefix #'string-prefix-p)
	(add-to-list 'matchs (semantic-tag-name tag) t)))
    matchs))

(defun company-async-semantic--find-tag (pos)
  (when-let ((file (file-truename (buffer-file-name)))
	     (tags (assoc-default file company-async-semantic--cache)))
    (seq-find (lambda (tag)
		(when-let* ((range (semantic-tag-overlay tag))
			    (start (aref range 0))
			    (end (aref range 1)))
		  (and (> pos start) (< pos end))))
	      tags)))

(defun company-async-semantic--get-args ()
  (when-let* ((tag (company-async-semantic--find-tag (point)))
	      (attr (semantic-tag-attributes tag)))
    (plist-get attr :arguments)))

(defun company-async-semantic--parse (start end)
  (let ((semantic--parse-table semantic-c-by--parse-table)
	(semantic-lex-syntax-modifications '((?> ".") (?< ".")))
	(semantic-lex-analyzer #'semantic-c-lexer))
    (semantic-lex-init)
    (semantic-parse-region start end 'bovine-inner-scope nil t)))

(defun company-async-semantic--get-local-vars ()
  (when-let* ((tag (company-async-semantic--find-tag (point)))
	      (range (semantic-tag-overlay tag))
	      (start (save-excursion
		       (goto-char (aref range 0))
		       (re-search-forward "{")))
	      (end (- (aref range 1) 1)))
    (company-async-semantic--parse start end)))

(defun company-async-semantic--get-all-local ()
  (delq nil (append (company-async-semantic--get-args)
		    (company-async-semantic--get-local-vars))))

(defun company-async-semantic--find-local (name)
  (seq-find (lambda (var)
	      (string= (semantic-tag-name var) name))
	    (company-async-semantic--get-all-local)))

(defun company-async-semantic--completions-local (prefix)
  (when-let ((vars (company-async-semantic--get-all-local)))
    (seq-filter (lambda (var)
		  (string-prefix-p prefix var))
		(mapcar #'semantic-tag-name vars))))

(defun company-async-semantic--completions-env (prefix)
  (delq nil (append (company-async-semantic--completions-local prefix)
		    (company-async-semantic--completions-system prefix))))

(defun company-async-semantic--find-bound ()
  (let ((line-start (line-beginning-position)))
    (save-excursion
      (if (re-search-backward ",\\|(" line-start t)
	  (+ (point) 1)
	line-start))))

(defun company-async-semantic--find-end (start)
  (save-excursion
    (re-search-backward company-async-semantic--member-regexp start)
    (point)))

(defun company-async-semantic--tokens (start end)
  (let ((str (buffer-substring-no-properties start end)))
    (mapcar #'string-trim
	    (split-string str company-async-semantic--member-regexp))))

(defun company-async-semantic--find-members (type)
  (let (match)
    (catch 'match
      (foreach-company-async-semantic--tag
	(when (and (string= (semantic-tag-name tag) type)
		   (plist-member (semantic-tag-attributes tag)
				 :members))
	  (setq match tag)))
      (plist-get (semantic-tag-attributes match) :members))))

(defun company-async-semantic--get-members (type tokens)
  (let ((members (company-async-semantic--find-members type)))
    (if tokens
	(when-let* ((token (pop tokens))
		    (match (seq-find (lambda (member)
				       (string= (semantic-tag-name member) token))
				     members))
		    (type (plist-get (semantic-tag-attributes match) :type)))
	  (company-async-semantic--get-members (semantic-tag-name type) tokens))
      (mapcar #'semantic-tag-name members))))

(defun company-async-semantic--completions-member ()
  (when-let* ((start (company-async-semantic--find-bound))
	      (end (company-async-semantic--find-end start))
	      (tokens (company-async-semantic--tokens start end))
	      (vars (company-async-semantic--get-all-local))
	      (match (seq-find (lambda (arg)
				 (string= (semantic-tag-name arg)
					  (car tokens)))
			       vars))
	      (type (plist-get (semantic-tag-attributes match) :type)))
    (when (listp type)
      (company-async-semantic--get-members (semantic-tag-name type)
					   (cdr tokens)))))

(defun company-async-semantic--completions-member-p (prefix)
  (and (equal prefix "") (looking-back company-async-semantic--member-regexp
				       (- (point) 2))))

(defun company-async-semantic--include (prefix dirs local)
  (let ((sub-dir (file-name-directory prefix))
	(regexp (concat "^" (file-name-nondirectory prefix)))
	(case-fold-search nil)
	candidates matchs cur-dir)
    (dolist (dir dirs)
      (setq cur-dir (concat dir sub-dir))
      (when (file-exists-p cur-dir)
	(dolist (result (directory-files cur-dir nil regexp))
	  (unless (string-match "^\\." result)
	    (if (file-directory-p (concat cur-dir result))
		(dolist (file (directory-files-recursively (concat cur-dir result) "^"))
		  (push (replace-regexp-in-string (concat cur-dir "\\(.*\\)")
						  (concat "\\1" local) file)
			matchs))
	      (push (concat result local) matchs))))
	(setq candidates (cl-union candidates matchs :test #'string=))))
    candidates))

(defun company-async-semantic--completions-include ()
  (let* ((start (line-beginning-position))
	 (str (buffer-substring-no-properties start (point)))
	 (local-dir (list (expand-file-name default-directory)))
	 (system-dirs company-async-semantic--default-path)
	 (local (string-match "#include \"" str))
	 (prefix (when (string-match company-async-semantic--include-regexp str)
		   (match-string 1 str))))
    (if local
	(company-async-semantic--include prefix local-dir "\"")
      (company-async-semantic--include prefix system-dirs ">"))))

(defun company-async-semantic--completions-include-p ()
  (let* ((start (line-beginning-position))
	 (str (buffer-substring-no-properties start (point))))
    (string-match company-async-semantic--include-regexp str)))

(defun company-async-semantic--update-cache (table file)
  (setq company-async-semantic--updating-cache t)
  (when-let ((tags (oref table tags)))
    (if (assoc file company-async-semantic--cache)
	(setcdr (assoc file company-async-semantic--cache) tags)
      (add-to-list 'company-async-semantic--cache (cons file tags))))
  (setq company-async-semantic--updating-cache nil))

(defun company-async-semantic--check-deps ()
  (let ((files-dep company-async-semantic--files-dep))
    ;; WORKAROUND:
    ;; When we get files dep on remote, it can be very slow.
    ;; Since we don't want to be stuck at every call, only check
    ;; `company-async-semantic--files-dep' for remote files.
    (unless (and (tramp-tramp-file-p default-directory)
		 company-async-semantic--files-dep)
      (company-async-semantic--get-files-dep))
    (cl-subsetp company-async-semantic--files-dep files-dep)))

(defun company-async-semantic--parse-success (files-parsed files-up-to-date)
  (company-async-semantic--set-status "Updating cache")
  (let (databases)
    (cl-macrolet ((update-cache (file)
		  `(let* ((filename (file-name-nondirectory ,file))
			  (cache-file (async-semantic--get-cache ,file))
			  (db (assoc-default cache-file databases))
			  table)
		     (unless db
		       (setq db (semanticdb-load-database cache-file))
		       (add-to-list 'databases (cons cache-file db)))
		     (setq table (seq-find (lambda (table)
					     (string= filename (oref table ,file)))
					   (oref db tables)))
		     (company-async-semantic--update-cache table ,file))))
      ;; files parsed
      (dolist (file files-parsed)
	(update-cache file))
      ;; files up-to-date
      (dolist (file files-up-to-date)
	(unless (assoc file company-async-semantic--cache)
	  (update-cache file)))
      ;; free ressources
      (dolist (db databases)
	(delete-instance (cdr db)))))
    (company-async-semantic--set-status nil))

(defun company-async-semantic--parse-fail ()
  (company-async-semantic--set-status nil))

(defun company-async-semantic--run-parse (&optional recursive)
  (async-semantic-buffer #'company-async-semantic--parse-success
			 #'company-async-semantic--parse-fail
			 recursive
			 company-async-semantic--default-path))

(defun company-async-semantic--parse-all ()
  (company-async-semantic--run-parse t)
  (company-async-semantic--set-status "Parse ongoing"))

(defun company-async-semantic--need-parse-all ()
  (or (not (assoc (file-truename (buffer-file-name))
		  company-async-semantic--cache))
      (not (company-async-semantic--check-deps))))

(defun company-async-semantic--candidates (arg)
  (let (candidates)
    (if (company-async-semantic--need-parse-all)
	(company-async-semantic--parse-all)
      (setq candidates
	    (cond
	     ;; include
	     ((company-async-semantic--completions-include-p)
	      (company-async-semantic--completions-include))
	     ;; member
	     ((company-async-semantic--completions-member-p arg)
	      (company-async-semantic--completions-member))
	     ;; env: function, variables, macro ...
	     (t (company-async-semantic--completions-env arg)))))
    candidates))

(defun company-async-semantic--grab-symbol ()
  (let ((member (company-grab-symbol-cons company-async-semantic--member-regexp 2)))
    (if (consp member)
	member
      (company-grab-symbol-cons company-async-semantic--include-regexp))))

(defun company-async-semantic--prefix ()
  (and company-async-semantic-enabled
       (memq major-mode company-async-semantic-modes)
       (not (company-in-string-or-comment))
       (not company-async-semantic--updating-cache)
       (or (company-async-semantic--grab-symbol) 'stop)))

(defun company-async-semantic--after-save ()
  (when (and (memq major-mode company-async-semantic-modes)
	     (not (company-async-semantic--need-parse))
	     (async-semantic-parse-idle))
    (company-async-semantic--run-parse)))

(defun company-async-semantic--enable ()
  (company-async-semantic--set-default-path)
  (local-set-key (kbd "M-.") #'company-async-semantic-goto-definition)
  (add-hook 'after-save-hook 'company-async-semantic--after-save nil t))

(defun company-async-semantic--disable ()
  (local-set-key (kbd "M-.") #'xref-find-definitions)
  (remove-hook 'after-save-hook 'company-async-semantic--after-save t))

(defun company-async-semantic--global-check ()
  (let ((func (if company-async-semantic-enabled
		  'company-async-semantic--enable
		'company-async-semantic--disable)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (memq major-mode company-async-semantic-modes)
	  (funcall func))))))

(defun company-async-semantic--setup ()
  (if company-async-semantic-enabled
      (company-async-semantic--enable)
    (company-async-semantic--disable)))

;;; External Functions

(defun company-async-semantic-goto-definition ()
  (interactive)
  (if company-async-semantic--updating-cache
      (message "Please wait, updating cache ...")
    (let* ((symbol (symbol-name (symbol-at-point)))
	   (tag (company-async-semantic--find-local symbol))
	   (file (file-truename (buffer-file-name))))
      (unless tag
	;; search over cache
	(let (match)
	  (catch 'match
	    (dolist (cache company-async-semantic--cache)
	      (when (member (car cache) company-async-semantic--files-dep)
		(dolist (cache-tag (cdr cache))
		  (when (company-async-semantic--match cache-tag symbol #'string=)
		    (setq file (car cache))
		    (setq tag cache-tag)
		    (setq match t))))))))
      ;; jump to the match
      (when (and file tag)
	(let* ((range (semantic-tag-overlay tag))
	       (start (aref range 0))
	       (end (aref range 1)))
	  (xref-push-marker-stack)
	  (with-current-buffer (find-file file)
	    (goto-char end)
	    (while (re-search-backward symbol start t))
	    (unless company-async-semantic--files-dep
	      (company-async-semantic--get-files-dep))))))))

(defun company-async-semantic-clear-cache ()
  (interactive)
  (setq company-async-semantic--cache nil))

(defun company-async-semantic-setup ()
  (if company-async-semantic-enabled
      (dolist (mode-hook (list 'c-mode-hook 'c++-mode-hook))
	(add-hook mode-hook 'company-async-semantic--setup))
    (dolist (mode-hook (list 'c-mode-hook 'c++-mode-hook))
      (remove-hook mode-hook 'company-async-semantic--setup)))
  (company-async-semantic--global-check))

(defun company-async-semantic-toggle ()
  (interactive)
  (setq company-async-semantic-enabled (not company-async-semantic-enabled))
  (message (concat "Company Async Semantic: " (if company-async-semantic-enabled
						  (propertize "Enabled" 'face 'success)
						(propertize "Disabled" 'face 'error))))
  (company-async-semantic-setup))

(defun company-async-semantic (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-async-semantic))
    (prefix (company-async-semantic--prefix))
    (candidates (company-async-semantic--candidates arg))
    (duplicates t)))

(provide 'company-async-semantic)
