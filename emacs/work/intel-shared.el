(defmacro save-excursion-if-not-at-point-max (buf &rest body)
  (declare (indent 1))
  `(if (= (point-max) (point))
       (progn ,@body
	      (when (get-buffer-window ,buf)
		(set-window-point (get-buffer-window ,buf) (point-max))))
     (save-excursion (progn ,@body))))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst icurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (interactive) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
		(arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defun dedicate-current-window (&optional undedicate)
  (interactive)
  (let ((dedicate (not undedicate)))
    (set-window-dedicated-p (get-buffer-window (current-buffer)) dedicate)))

(defun undedicate-current-window ()
  (interactive)
  (dedicate-current-window t))

(defun delete-other-line-occurences ()
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	(count 0))
    (save-excursion
      (forward-line 1)
      (while (search-forward line nil t)
	(incf count)
	(delete-region (line-beginning-position) (1+ (line-end-position)))))
    (message "Line '%s' found and deleted %d times." line count)))

(provide 'intel-shared)
