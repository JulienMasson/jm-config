;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                IFWI TOOLS                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ifwi PATH
(defvar IAFW_PATH "/home/lab/Documents/Intel/mcg_umfdk-umfdk/")
(defvar STITCH_PATH "/home/lab/Documents/Intel/ndg_ifwi-marvin/marvin/")
(defvar IFWI_DEV_PATH "/home/lab/Documents/Intel/marvin/")
(defvar RELEASE_PATH "/ssh:tllabx2:/build/jmassonx/ndg-android/vendor/intel/PRIVATE/mvn/fw/")

;; ifwi vars
(defvar iafw-targets-list '("robby" "grant" "glacier"))
(defvar ifwi-targets-list '("robby" "anthracite" "mars" "shasta" "grant" "glacier"))
(defvar copy-binaries-list '((" IAFW -> IFWI "    . copy-iafw-ifwi-binaries)
                             (" IFWI -> Release " . copy-ifwi-release-binaries)))
(defvar release-ifwi-buffer "*release-ifwi*")
(defvar process-ifwi-index 0)
(defvar release-ifwi-time 0)

;; ifwi release
(defstruct process-ifwi
  header  ; header print before executing cmd
  path    ; path to run cmd
  cmd     ; command to execute
  func    ; callback func after execution cmd done
  args)   ; args for callback func

(defvar process-ifwi-list '())
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nBuilding IAFW for Glacier\n"
                                :path IAFW_PATH
                                :cmd "./build_marvin.sh VARIANT=glacier"
                                :func 'copy-iafw-ifwi-binaries
                                :args "glacier") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nStitching IFWI for Glacier"
                                :path STITCH_PATH
                                :cmd "./stitch.sh glacier"
                                :func 'copy-ifwi-release-binaries
                                :args "glacier") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nBuilding IAFW for Grant\n"
                                :path IAFW_PATH
                                :cmd "./build_marvin.sh VARIANT=grant"
                                :func 'copy-iafw-ifwi-binaries
                                :args "grant") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nStitching IFWI for Grant"
                                :path STITCH_PATH
                                :cmd "./stitch.sh grant"
                                :func 'copy-ifwi-release-binaries
                                :args "grant") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nBuilding IAFW for Robby\n"
                                :path IAFW_PATH
                                :cmd "./build_marvin.sh VARIANT=robby"
                                :func 'copy-iafw-ifwi-binaries
                                :args "robby") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nStitching IFWI for Robby"
                                :path STITCH_PATH
                                :cmd "./stitch.sh robby"
                                :func 'copy-ifwi-release-binaries
                                :args "robby") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nStitching IFWI for Anthracite"
                                :path STITCH_PATH
                                :cmd "./stitch.sh anthracite"
                                :func 'copy-ifwi-release-binaries
                                :args "anthracite") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nStitching IFWI for Shasta"
                                :path STITCH_PATH
                                :cmd "./stitch.sh shasta"
                                :func 'copy-ifwi-release-binaries
                                :args "shasta") t)
(add-to-list 'process-ifwi-list
             (make-process-ifwi :header "\n\nStitching IFWI for Mars"
                                :path STITCH_PATH
                                :cmd "./stitch.sh mars"
                                :func 'copy-ifwi-release-binaries
                                :args "mars") t)

;; functions
(defun copy-binaries (target)
  (interactive (list (ido-completing-read "Copy binairies: "
					  (mapcar 'car copy-binaries-list)
					  nil t nil nil)))
  (let ((t-or-f (assoc-default target copy-binaries-list)))
    (if (functionp t-or-f)
        (call-interactively t-or-f))))
  
(defun copy-iafw-ifwi-binaries (target)
  (interactive (list (ido-completing-read "Target: "
					  iafw-targets-list
					  nil t nil nil)))
  (copy-file (concat IAFW_PATH "brd_merrifield/elf/ia32fw.bin")
             (concat STITCH_PATH "/ia/generated/ia32fw-" target ".bin") t)
  (copy-file (concat IAFW_PATH "bin_valhooks/merrifield/OemHooks_merrifield.bin")
               (concat STITCH_PATH "/ia/generated/OemHooks_merrifield-" target ".bin") t))

(defun copy-ifwi-release-binaries (target)
  (interactive (list (ido-completing-read "Target: "
					  ifwi-targets-list
					  nil t nil nil)))
  (copy-file (concat STITCH_PATH "marvin_dnx_fwr.bin")
             (concat RELEASE_PATH target "/") t)
  (copy-file (concat STITCH_PATH "marvin_dnx_osr.bin")
             (concat RELEASE_PATH target "/") t)
  (copy-file (concat STITCH_PATH "marvin_ifwi-dbg.bin")
             (concat RELEASE_PATH target "/") t)
  (copy-file (concat STITCH_PATH "marvin_ifwi.bin")
             (concat RELEASE_PATH target "/") t))

(defun copy-ifwi-binaries-dev (path)
  (copy-file (concat STITCH_PATH "marvin_dnx_fwr.bin")
	     (concat IFWI_DEV_PATH path) t)
  (copy-file (concat STITCH_PATH "marvin_dnx_osr.bin")
	     (concat IFWI_DEV_PATH path) t)
  (copy-file (concat STITCH_PATH "marvin_ifwi.bin")
	     (concat IFWI_DEV_PATH path) t))

(defun compile-ifwi (target)
  (interactive (list (ido-completing-read "IAFW target: "
					  iafw-targets-list
					  nil t nil nil)))
  (let ((default-directory IAFW_PATH))
    (compile (concat "./build_marvin.sh VARIANT=" target))))

(defun stitch-ifwi (target)
  (interactive (list (ido-completing-read "IFWI target: "
					  ifwi-targets-list
					  nil t nil nil)))
  (let ((default-directory STITCH_PATH))
    (async-shell-command (concat "./stitch.sh " target))))

(defun flash-ifwi ()
  (interactive)
  (copy-ifwi-binaries-dev "ifwi/")
  (let ((default-directory (concat IFWI_DEV_PATH "ifwi/")))
    (async-shell-command "platformflashtool-cli-launcher.sh -f flash-IFWI-only.xml")))

(defun blankphone-ifwi ()
  (interactive)
  (copy-ifwi-binaries-dev "blankphone/")
  (let ((default-directory (concat IFWI_DEV_PATH "blankphone/")))
    (async-shell-command "platformflashtool-cli-launcher.sh -f flash.xml")))

(defun process-ifwi-sentinel (p e)
  (with-current-buffer (process-buffer p)
    (if (string= e "finished\n")
        (let* ((target (nth process-ifwi-index process-ifwi-list))
               (func (process-ifwi-func target))
               (args (process-ifwi-args target)))
          ;; (sleep-for 1)
          (if func (funcall func args))
          (setq process-ifwi-index (1+ process-ifwi-index))
          (if (< process-ifwi-index (length process-ifwi-list))
              (process-ifwi-run)
            (with-current-buffer release-ifwi-buffer
              (goto-char (point-max))
              (insert (propertize "\n\nRELEASE done in:  " 'face 'success))
              (insert (format-time-string
                       "%M mins %S secs"
                       (time-subtract (current-time) release-ifwi-time)))))))))

(defun process-ifwi-run ()
  (let* ((tramp-verbose 0)
         (target (nth process-ifwi-index process-ifwi-list))
         (default-directory (process-ifwi-path target))
         (cmd (process-ifwi-cmd target))
         (header (process-ifwi-header target)))
    (with-current-buffer release-ifwi-buffer
      (goto-char (point-max))
      (insert (propertize header 'face 'success)))
    (set-process-sentinel
     (start-file-process "release-ifwi" release-ifwi-buffer "bash" "-c" cmd)
     'process-ifwi-sentinel)))

(defun release-ifwi ()
  (interactive)
  (when (get-buffer release-ifwi-buffer)
      (kill-buffer release-ifwi-buffer))
  (generate-new-buffer release-ifwi-buffer)
  (setq process-ifwi-index 0)
  (setq release-ifwi-time (current-time))
  (process-ifwi-run))

(defvar ifwi-cmds
  '(("compile"			.       compile-ifwi)
    ("copy"			.       copy-binaries)
    ("stitch"			.       stitch-ifwi)
    ("flash"	                .	flash-ifwi)
    ("blankphone"	        .	blankphone-ifwi)
    ("release"	                .	release-ifwi)))

(defun ifwi-tools (cmds)
  (interactive (list (ido-completing-read "IFWI tools: "
					  (mapcar 'car ifwi-cmds)
					  nil t nil nil)))
  (let ((t-or-f (assoc-default cmds ifwi-cmds)))
    (if (functionp t-or-f)
	(call-interactively t-or-f))))

(provide 'ifwi-tools)
