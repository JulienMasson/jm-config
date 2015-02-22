(require 'pm-android)

(defvar pm-intel-android-targets
  '(("droidboot"	.	"droidbootimage")
    ("bootloader"	.	"bootloaderimage")
    ("blankphone"	.	"blank_flashfiles")
    ("flashfiles"	.	"flashfiles")))

(defun pm-intel-android-compile ()
  (interactive)
  (let ((pm-android-targets (append pm-android-targets pm-intel-android-targets)))
    (call-interactively 'pm-android-compile)))

(defun pm-intel-android-mixins-update ()
  (interactive)
  (let ((default-directory (concat aosp-path "/")))
    (compile "for file in $(find ./device -name mixins.spec) ;\
              do ./device/intel/mixins/mixin-update -s $file; done")))

(defun pm-intel-android-kernelflinger ()
  (interactive)
  (let ((default-directory (concat aosp-path "/")))
    (compile (concat (pm-android-load-compile-env)
		     "cd hardware/intel/kernelflinger/ "
		     "&& ./generate-prebuilts.sh"))
    (with-current-buffer "*compilation*"
      (setq default-directory (concat aosp-path
				      "/hardware/intel/kernelflinger")))))

(defun pm-intel-android-find-file-hook ()
  (let ((file-name (buffer-file-name)))
    (when (and file-name
	       (file-exists-p file-name)
	       (file-writable-p file-name))
      (toggle-read-only t))))

(pm-register-backend
 (make-pm-backend :name "intel-android"
		  :open-hook 'pm-android-open-hook
		  :find-file 'pm-android-find-file
		  :find-file-hook 'pm-intel-android-find-file-hook
		  :compile 'pm-intel-android-compile))

(provide 'pm-android-intel)
