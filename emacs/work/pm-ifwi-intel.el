(require 'project-manager)

(pm-register-backend
 (make-pm-backend :name "ifwi-intel"
		  :find-file 'project-find-file-subproject))

(provide 'pm-ifwi-intel)
