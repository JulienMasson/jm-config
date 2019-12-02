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

(defvar company-async-semantic--cache nil)

(defvar-local company-async-semantic--default-path nil)

(defvar-local company-async-semantic--files-dep nil)

(defvar company-async-semantic--parsing-ongoing nil)

;;; Internal Functions

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

(defun company-async-semantic--get-tags (files)
  (let (tags)
    (dolist (file files)
      (when-let ((tag (assoc-default file company-async-semantic--cache)))
	(setq tags (append tags tag))))
    tags))

(defun company-async-semantic--match (tag prefix compare)
  (let ((name (semantic-tag-name tag))
	(class (semantic-tag-class tag)))
    (and (or (eq class 'function)
	     (eq class 'variable)
	     (eq class 'type))
	 (funcall compare prefix name))))

(defun company-async-semantic--completions-system (prefix)
  (when-let* ((files company-async-semantic--files-dep)
	      (tags (company-async-semantic--get-tags files))
	      (matchs (seq-filter (lambda (tag)
				    (company-async-semantic--match tag prefix
								   #'string-prefix-p))
				  tags)))
    (mapcar #'semantic-tag-name matchs)))

(defun company-async-semantic--completions-scope (prefix)
  )

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

(defun company-async-semantic--all-local-type (name)
  (let ((vars (company-async-semantic--get-all-local)))
    (seq-find (lambda (var)
		(string= (semantic-tag-name var)
			 symbol))
	      vars)))

(defun company-async-semantic--completions-local (prefix)
  (when-let ((vars (company-async-semantic--get-all-local)))
    (seq-filter (lambda (var)
		  (string-prefix-p prefix var))
		(mapcar #'semantic-tag-name vars))))

(defun company-async-semantic--completions-env (prefix)
  (delq nil (append (company-async-semantic--completions-local prefix)
		    (company-async-semantic--completions-scope prefix)
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
  (when-let* ((files company-async-semantic--files-dep)
	      (tags (company-async-semantic--get-tags files))
	      (match (seq-find (lambda (tag)
				 (string= (semantic-tag-name tag) type))
			       tags)))
    (plist-get (semantic-tag-attributes match) :members)))

(defun company-async-semantic--get-members (type tokens)
  (let ((members (company-async-semantic--find-members type)))
    (if tokens
	(when-let* ((token (pop tokens))
		    (match (seq-find (lambda (member)
				       (string= (semantic-tag-name member)
						token))
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

(defun company-async-semantic--include (prefix dirs)
  (let ((regexp (concat "^" prefix))
	(case-fold-search nil)
	candidates matchs)
    (dolist (dir dirs)
      (setq matchs (mapcar (lambda (match)
			     (replace-regexp-in-string dir "" match))
			   (directory-files-recursively dir regexp)))
      (setq candidates (cl-union candidates matchs :test #'string=)))
    candidates))

(defun company-async-semantic--completions-include (prefix)
  (let* ((start (line-beginning-position))
	 (str (buffer-substring-no-properties start (point)))
	 (local-dir (list (expand-file-name default-directory)))
	 (system-dirs company-async-semantic--default-path))
    (if (string-match "#include <" str)
	(company-async-semantic--include prefix system-dirs)
      (company-async-semantic--include prefix local-dir))))

(defun company-async-semantic--completions-include-p ()
  (let* ((start (line-beginning-position))
	 (str (buffer-substring-no-properties start (point))))
    (string-match "#include [\"<]" str)))

(defun company-async-semantic--update-cache (file)
  (when-let* ((filename (file-name-nondirectory file))
	      (dir (file-name-directory file))
	      (cache-file (semanticdb-cache-filename semanticdb-new-database-class dir))
	      (db (semanticdb-load-database cache-file))
	      (table (seq-find (lambda (table)
				 (string= filename (oref table file)))
			       (oref db tables)))
	      (tags (oref table tags)))
    (if (assoc file company-async-semantic--cache)
	(setcdr (assoc file company-async-semantic--cache) tags)
      (add-to-list 'company-async-semantic--cache (cons file tags)))))

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

(defun company-async-semantic--parse-done (files)
  (mapc #'company-async-semantic--update-cache files)
  (setq company-async-semantic--parsing-ongoing nil))

(defun company-async-semantic--run-parse (&optional recursive)
  (setq company-async-semantic--parsing-ongoing t)
  (async-semantic-buffer #'company-async-semantic--parse-done recursive))

(defun company-async-semantic--need-parse ()
  (not (assoc (file-truename (buffer-file-name))
	      company-async-semantic--cache)))

(defun company-async-semantic--candidates (arg)
  (let (candidates)
    (if (or (company-async-semantic--need-parse)
	    (not (company-async-semantic--check-deps)))
	(company-async-semantic--run-parse t)
      (setq candidates
	    (cond
	     ;; include
	     ((company-async-semantic--completions-include-p)
	      (company-async-semantic--completions-include arg))
	     ;; member
	     ((company-async-semantic--completions-member-p arg)
	      (company-async-semantic--completions-member))
	     ;; env: function, variables, macro ...
	     (t (company-async-semantic--completions-env arg)))))
    candidates))

(defun company-async-semantic--grab-symbol ()
  (company-grab-symbol-cons company-async-semantic--member-regexp 2))

(defun company-async-semantic--prefix ()
  (and company-async-semantic-enabled
       (memq major-mode company-async-semantic-modes)
       (not (company-in-string-or-comment))
       (not company-async-semantic--parsing-ongoing)
       (or (company-async-semantic--grab-symbol) 'stop)))

(defun company-async-semantic--after-save ()
  (when (and (memq major-mode company-async-semantic-modes)
	     (not (company-async-semantic--need-parse)))
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
  (let* ((symbol (symbol-name (symbol-at-point)))
	 (files company-async-semantic--files-dep)
	 (local (company-async-semantic--all-local-type symbol))
	 file tag)
    (if local
	;; search over local vars
	(progn
	  (setq file (file-truename (buffer-file-name)))
	  (setq tag local))
      ;; search over cache
      (when-let ((match (seq-find (lambda (cache)
				    (seq-find (lambda (tag)
						(company-async-semantic--match
						 tag symbol #'string=))
					      (cdr cache)))
				  company-async-semantic--cache))
		 (tags (cdr match))
		 (tmp (seq-find (lambda (tag)
				  (company-async-semantic--match tag symbol
								 #'string=))
				tags)))
	(setq file (car match))
	(setq tag tmp)))
    ;; jump to the match
    (when (and file tag)
      (let* ((range (semantic-tag-overlay tag))
	     (start (aref range 0))
	     (end (aref range 1)))
	(xref-push-marker-stack)
	(with-current-buffer (find-file file)
	  (goto-char end)
	  (re-search-backward symbol start t))))))

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
