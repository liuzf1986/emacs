;; The plugin use to config
;  auto-complete-c-header
;  auto-complete-clang-async
;  rtags
;
;;  pcurr-proj
;;  pproj
;;  (pname "any" :type string)
;;  (ptype "gcc" :type string)
;;  (pcust-inc '() :type list)
;(setf (pproj-pname pcurr-proj) "test")
;(setf (pproj-ptype pcurr-proj) "gcc")
;(setf (pproj-pcust-inc pcurr-proj)
;	  '("include"))

(require 'cl)
(require 'mutils)

(defstruct pproj
  "define mproj configuration"
  (pname "any" :type string)
  (ptype "gcc" :type string)
  (pcust-inc '() :type list)
  )

;; define const string
(defconst gcc "gcc")
(defconst ndk "ndk")
(defconst mproj-conf ".mproj.pro")
(defconst proj-template 
"\;(setf (pproj-pname pcurr-proj) \"test\")\n\
\;(setf (pproj-ptype pcurr-proj) \"gcc\")\n\
\;(setf (pproj-pcust-inc pcurr-proj) \'(\"include\"))"
)
;; define pcurr-proj variable, this will modified by configuration file.
(defvar-local proj-file-path nil)
(setq-default pcurr-proj (make-pproj))
(defvar-local include-paths '())
(defvar-local rtags-parse nil)


;; --------------------------- initial gcc and ndk headers ---------------------------
(defun ede-include-path ()
  "Return ede project include path. Cedet just have function 
@ede-system-include-path, by I need spacified include path too !!"
  (when (ede-directory-project-p (get-current-path))
    (oref ede-object-root-project :include-path)))

(defun ede-all-include-path ()
  "Return all inlcude path that defined in @ede-cpp-root-project 
when current buffer is managed by ede project."
  (if ede-object-project
    (let* ((spec-incs (oref ede-object-root-project :include-path))
           (root-path (directory-file-name (ede-project-root-directory ede-object-project)))
           (obs-spec-incs (mapcar (lambda (arg) (concat root-path arg)) spec-incs)))
      (append obs-spec-incs (ede-system-include-path ede-object))
      )
	gcc-headers
		))

(defun rtags-setup-compiler (real-compiler)
  "Setup compiler with @gcc-rtags-wrapper-spec.sh"
  ;; Get real compiler path.
  (when (file-executable-p real-compiler)
	(shell-command (concat "mv " real-compiler " " real-compiler emacs-rtags-compile-postfix))
    (shell-command (concat "ln -s " emacs-bin-path "/rtags/gcc-rtags-wrapper-spec.sh" " " real-compiler))))

(defun rtags-restore-compiler (real-compiler)
  "Restore compiler from symbol link @gcc-rtags-wrapper-spec.sh"
  (when (and (file-symlink-p real-compiler) (file-exists-p (concat real-compiler emacs-rtags-compile-postfix)))
    (shell-command (concat "rm -f " real-compiler))
    (shell-command (concat "mv " real-compiler emacs-rtags-compile-postfix " " real-compiler))))

(defun rtags-build (build-cmd compilers)
  "Build with rtags apply. 
@build-cmd is command will executed, 
@compiler is path or compile command that has added to path evironment"
  (mapcar (lambda (arg) (rtags-setup-compiler arg)) compilers)
  (shell-command build-cmd "rtags-build" "rtags-build")
  (mapcar (lambda (arg) (rtags-restore-compiler arg)) compilers)
)

(defun ndk-build0 (arch host)
  "ndk-build with specified arch and host.
Be aware, this function has't valified on Windows system"
  (let* ((ndk-path (file-name-directory (shell-command-to-string "which ndk-build")))
         (bin-path (file-name-as-directory (concat ndk-path "toolchains/" arch "/prebuilt/" host "/bin")))
         (compilers '("arm-linux-androideabi-cpp" "arm-linux-androideabi-g++" "arm-linux-androideabi-gcc"))
         )
    (when (file-directory-p bin-path)
      (setq compilers (mapcar (lambda (compiler) (concat bin-path compiler)) compilers))
      (rtags-build "ndk-build" compilers))))

(defun ndk-build ()
  "ndk-build"
  (interactive)
  (ndk-build0 "arm-linux-androideabi-4.8" "linux-x86_64"))


(defun get-g++-include-path ()
  "Execute g++ -E -xc++ -v - and parse to get current g++ include path"
  (let* ((command-result (shell-command-to-string "echo \"\" | g++ -v -x c++ -E -"))
         (start-string "#include <...> search starts here:\n")
         (end-string "End of search list.\n")
         (start-pos (string-match start-string command-result))
         (end-pos (string-match end-string command-result))
         (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
    (split-string include-string)))

(defun get-ndk-include-path (ver)
  "Get ndk include folders"
  (let* ((ndk-root-path (shell-command-to-string "which ndk-build"))
         (ndk-root-path (trim ndk-root-path))
         (ndk-version (concat "android-" (number-to-string ver)))
         )
	(if (> (length ndk-root-path) 0)
        (let* ((ndk-platforms-path (concat (file-name-directory ndk-root-path) "platforms/"))
              (ndk-platform-path (concat ndk-platforms-path ndk-version))
              (ndk-include-path (concat ndk-platform-path "/arch-arm/usr/include")))
          (if (file-exists-p ndk-include-path)
              ndk-include-path
            nil
              )
          ))))

(defconst gcc-headers (get-g++-include-path))
(defconst ndk-headers (cons (get-ndk-include-path 19) nil))
;; --------------------------- initial gcc and ndk headers ---------------------------

(defun mproj:get-include-paths ()
  include-paths
  )

(defun mproj:load-config ()
  (setq proj-file-path (find-file-upward mproj-conf))
  (if proj-file-path 
	(load-file proj-file-path)
	(write-region proj-template nil (expand-file-name mproj-conf (directory-file-name (file-name-directory (buffer-file-name)))))
	)
  )

(defun mproj:update-ptype (&optional ptype)
  "update header paths with ptype"
  (unless ptype
	(setq ptype gcc)
	)

  (cond 
   ((string-equal ptype gcc)
	(setq include-paths gcc-headers)
	)
   ((string-equal ptype ndk)
	(setq include-paths ndk-headers)
	)
   (t 
	(setq include-paths (get-g++-include-path))
	)
   )
)

(defun mproj:update-pcust-incs (&optional custinc)
  "update header paths with custom addon"
  (if proj-file-path
	  ;; project file exist
	  (when (and custinc (> (list-length custinc) 0))
		(let* ((proj-root-path (directory-file-name (file-name-directory proj-file-path))))
		  (mapcar (lambda (path) 
					(setq include-paths (cons (directory-file-name (expand-file-name path proj-root-path)) include-paths)))
				  custinc)
		  )
		)
	)
  (setq include-paths (cons (directory-file-name (file-name-directory buffer-file-name)) include-paths))
  )

(defun mproj:apply-ac-c-header ()
  (when (and (featurep 'auto-complete-c-headers) (> (list-length include-paths) 0))
	(add-to-list 'ac-sources 'ac-source-c-headers)
	(setq achead:include-directories include-paths)
	)
)

(defun mproj:apply-ac-clang-async ()
  (when (and (featurep 'auto-complete-clang-async) (> (list-length include-paths) 0))
	(setq ac-clang-cflags (mapcar (lambda (path) (concat "-I" path)) include-paths))
	(setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags))
	(ac-clang-update-cmdlineargs)
	(setq ac-sources '(ac-source-clang-async))
	)
  )

(defun get-update-rtags-cmd-string ()
  (let* ((cmd-cflag "rc --compile ")
		 (source (buffer-file-name))
		 )
	(setq cmd-cflag (concat cmd-cflag "\"")) ;start with quoit
	(setq cmd-cflag (concat cmd-cflag "g++ -Wall --std=c++11 ")) ;Wall
	(when (string-equal (pproj-ptype pcurr-proj) ndk)
	  (setq cmd-cflag (concat cmd-cflag "-DANDROID"))
	  )
	(mapcar (lambda (path) (setq cmd-cflag (concat cmd-cflag "-I" path " "))) include-paths) ;add include paths
	(setq cmd-cflag (concat cmd-cflag source)) ;add source
	(setq cmd-cflag (concat cmd-cflag "\" > /dev/null 2>&1 &")) ;end with quoit
	)
  )

(defun mproj:display-diagnostice-maybe ()
  (let* ((errors (if rtags-error-warning-count (car rtags-error-warning-count) 0))
		 (warnings (if rtags-error-warning-count (cdr rtags-error-warning-count) 0)))
	(message (number-to-string errors))
	(message (number-to-string warnings))
	(when (or (> errors 0) (> warnings 0))
	  (switch-to-buffer-other-window "*RTags Diagnostics*")
	  )
	)
  )

(defun mproj:after-save-hook ()
  ;; when current buffer is a c souce file, update rtags
  (when (and (featurep 'rtags) (member (file-name-extension buffer-file-name) '("cpp" "c" "cc")))
	(let ((rc-cmd (get-update-rtags-cmd-string)))
;	  (async-shell-command rc-cmd)
	  (shell-command-to-string rc-cmd)
;	  (add-hook 'rtags-diagnostics-hook 'mproj:display-diagnostice-maybe)
	  (rtags-diagnostics)
	  (mproj:display-diagnostice-maybe)
	  )
	)
)

(defun mproj:apply-rtags ()
  (add-hook 'after-save-hook 'mproj:after-save-hook)
)

(defun mproj:apply-proj ()
  (interactive)

  (when (equal (list-length include-paths) 0)
	(mproj:load-config)
	(mproj:update-ptype (pproj-ptype pcurr-proj))
	(mproj:update-pcust-incs (pproj-pcust-inc pcurr-proj))
	;; remove duplicated include path
	(setq include-paths (delete-dups include-paths))
	;; configuration real features
	(mproj:apply-ac-clang-async)
	(mproj:apply-ac-c-header)
	(mproj:apply-rtags)
	)
)

(provide 'mproj)
