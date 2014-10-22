;; --------------------------- define const path ---------------------------
(defun get-current-path ()
  "Get current buffer's path."
  (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-base-config-path (get-current-path))
(defconst emacs-plugin-path (concat emacs-base-config-path "/plugins"))
(defconst emacs-libs-path (concat emacs-base-config-path "/libs"))
(defconst emacs-relevance-path (concat emacs-base-config-path "/relevance"))
(defconst emacs-bin-path (concat emacs-base-config-path "/bin"))
(defconst emacs-rtags-compile-postfix "-rtags")
;; --------------------------- define variables ----------------------------

;; ------------------------------- utils  ----------------------------------
(defun trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun trim (s)
  "Remove whitespace at the beginning and end of S."
  (trim-left (trim-right s)))

(defun list-subfolders (parent)
  "Get subfolders name"
  (let* ((files-list '())
	 (current-directory-list
	  (directory-files parent t)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
	(if (and f (file-directory-p f))
	    (setq files-list (cons f files-list))))
      (setq current-directory-list (cdr current-directory-list)))
    files-list
    ))

(defun append-load-paths (paths)
  "Add path in @paths to @load-path."
  (while paths
    (let ((f (car paths)))
      (add-to-list 'load-path f)
      (message "add to paths" f))
    (setq paths (cdr paths))))

(defun append-subdirs-to-load-path (folder)
  "Add @folder's sub folders to @load-path "
  (let ((subdirs (list-subfolders folder)))
    (while subdirs
      (let ((subdir (car subdirs)))
		(add-to-list 'load-path subdir))
      (setq subdirs (cdr subdirs)))))

(defun toggle-full-screen ()
  "Swich window to full screen or restore."
  (interactive)
  (if (display-graphic-p)
	  (if (eq system-type 'gnu/linux)
		  (progn
			(x-send-client-message
			 nil 0 nil "_NET_WM_STATE" 32
			 '(2 "_NET_WM_STATE_FULLSCREEN" 0))))))

(defun toggle-maximize-screen ()
  "Switch window to maximize or restore"
  (interactive)
  (if (display-graphic-p)
	  (if (eq system-type 'gnu/linux)
		  (progn
			(x-send-client-message
			 nil 0 nil "_NET_WM_STATE" 32
			 '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
			(x-send-client-message
			 nil 0 nil "_NET_WM_STATE" 32
			 '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0))))))

(defun find-file-upward (regrex &optional basedir)
  "从basedir开始向上查找文件regrex"
  (unless basedir ;; 未设置目录，取当前目录
	(setq basedir (file-name-directory (or load-file-name buffer-file-name)))
	)
  (let* ((current-dir (file-name-as-directory basedir))
		 )
	(if (file-exists-p (concat current-dir regrex))
		(concat (file-name-as-directory basedir) regrex)
	  ;; 递归查找上层目录
	  (unless (equal "/" basedir)
		(find-file-upward regrex (file-name-directory (directory-file-name current-dir)))))))

(defun ede-include-path ()
  "Return ede project include path. Cedet just have function 
@ede-system-include-path, by I need spacified include path too !!"
  (when (ede-directory-project-p (get-current-path))
    (oref ede-object-root-project :include-path)))

(defun ede-all-include-path ()
  "Return all inlcude path that defined in @ede-cpp-root-project 
when current buffer is managed by ede project."
  (when ede-object-project
    (let* ((spec-incs (oref ede-object-root-project :include-path))
           (root-path (directory-file-name (ede-project-root-directory ede-object-project)))
           (obs-spec-incs (mapcar (lambda (arg) (concat root-path arg)) spec-incs)))
      (append obs-spec-incs (ede-system-include-path ede-object))
      )))

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

(setq window-system-default-frame-alist
      '(
        (x
		 (menu-bar-lines . 1)
		 (tool-bar-lines . nil)
		 (mouse-wheel-mode . 1)
		 (mouse-wheel-follow-mouse . t)
		 (mouse-avoidance-mode . 'exile)
		 (font . "YaHei Consolas Hybrid:pixelsize=15:antialias=subpixel")
		 )
        (nil
		 (menu-bar-lines . 0) (tool-bar-lines . 0))))
;; ------------------------------- utils  ----------------------------------

;; ------------------------- Customize include path ------------------------
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
              (ndk-include-path (concat ndk-platform-path "/arch-arm/usr/include/")))
          (if (file-exists-p ndk-include-path)
              ndk-include-path
            nil
              )
          ))))

(defconst gcc-headers (get-g++-include-path))
(defconst ndk-headers (cons (get-ndk-include-path 19) nil))
;; ------------------------- Customize include path ------------------------

;;------------------------- Customize configuration ------------------------
;; default UTF-8
(set-language-environment 'utf-8)
;; Show current time
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; line space
(setq default-line-spacing 1)
;; charactors per line in c/c++
(setq default-fill-column 120)
;; default major mode
(setq default-major-mode 'text-mode)
;; kill ring length
(setq kill-ring-max 200)
;; force new line with a file
(setq require-final-newline t)
;; sytax hilight
(global-font-lock-mode 1)
;; highlight selection region
(transient-mark-mode t)
;; show line number and column number
(global-linum-mode t)
(column-number-mode t)
;; high light paren
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;; share clipboard
(setq x-select-enable-clipboard t)
;; disable inhibit
(setq inhibit-startup-message t)
;; cursor type bar
(setq default-cursor-type 'bar)
;; tab with four space
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list ())
;; disable toolbar
(tool-bar-mode -1)
(menu-bar-mode t)
(scroll-bar-mode -1)
;; disable bell
(setq visible-bell t)
;; replace y and n for yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; resize mini window
(setq resize-mini-windows t)
;; recursive mini buffer
(setq enable-recursive-minibuffers t)
;; don't backup
(setq make-backup-files nil)
;; split windows virtical
(setq split-width-threshold nil)
;; color theme
(load-theme 'tango-dark)
;; maximize windows when startup
(toggle-maximize-screen)
;; enter debug mode when error occur
(toggle-debug-on-quit)
;; display buffer name on title bar
(if window-system
	(setq frame-title-format
		  '((:eval (if (buffer-modified-p) " * " " "))
			(:eval (abbreviate-file-name (buffer-file-name))))))
;;------------------------- Customize configuration ------------------------

;; ---------------------------- key binds  ---------------------------------
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-*") 'evil-search-symbol-forward)
(global-set-key (kbd "C-#") 'evil-search-symbol-backward)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3)))
;; ---------------------------- key binds  ---------------------------------



;; Add plugins path to @load-path
(add-to-list 'load-path emacs-base-config-path)
(append-subdirs-to-load-path emacs-plugin-path)
(append-subdirs-to-load-path emacs-libs-path)

;; --------------------------- emacs feathers ------------------------------
(global-ede-mode t)
;; --------------------------- emacs feathers ------------------------------
;; --------------------------- plugins feathres ----------------------------
(require 'plugins-manager)
(enable-ibus)
;(enable-color-theme)
;(enable-sr-speed-bar)
(enable-dirtree)
(enable-yasnippet)
;(enable-window-numbering)
(enable-highlight-symbol)
(enable-auto-pair)
(enable-auto-complete)
(enable-auto-complete-clang-async)
(enable-auto-complete-c-header)
;(enable-cscope)
;(enable-smart-compile)
;(enable-ztree)
(enable-clang-format)
(enable-shell-switcher)
(enable-rtags)
;; --------------------------- plugins feathers ----------------------------


