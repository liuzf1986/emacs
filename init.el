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
(menu-bar-mode -1)
(scroll-bar-mode t)
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
;; display buffer name on title bar
(setq frame-title-format '((:eval default-directory)))
;; enter debug mode when error occur
;; enable mouse for terminal mode
(xterm-mouse-mode)
;(toggle-debug-on-quit)
(setq window-system-default-frame-alist
      '(
        (x
;		 (menu-bar-lines . 1)
;		 (tool-bar-lines . nil)
		 (mouse-wheel-mode . 1)
		 (mouse-wheel-follow-mouse . t)
		 (mouse-avoidance-mode . 'exile)
		 (font . "YaHei Consolas Hybrid:pixelsize=15:antialias=subpixel")
		 )
       (nil
		 (menu-bar-lines . 0) (tool-bar-lines . 0))))
(custom-set-faces
  '(flymake-errline ((((class color)) (:background "LightPink"))))
  '(flymake-warnline ((((class color)) (:background "LightBlue2")))))
;;------------------------- Customize configuration ------------------------

;; ---------------------------- key binds  ---------------------------------
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-*") 'evil-search-symbol-forward)
(global-set-key (kbd "C-#") 'evil-search-symbol-backward)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3)))
;; ---------------------------- key binds  ---------------------------------

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

;; --------------------------- load my utilityes ----------------------------

;; Add plugins path to @load-path
(add-to-list 'load-path emacs-base-config-path)
(require 'mutils)

(append-subdirs-to-load-path emacs-plugin-path)
(append-subdirs-to-load-path emacs-libs-path)
;; maximize windows when startup
(toggle-maximize-screen)
;; --------------------------- load my utilityes ----------------------------

;; --------------------------- emacs feathers ------------------------------
(global-ede-mode t)
;; --------------------------- emacs feathers ------------------------------
;; --------------------------- plugins feathres ----------------------------
(require 'plugins-manager)

(enable-ibus)
;(enable-color-theme)
;(enable-sr-speed-bar)
;(enable-dirtree)
(enable-yasnippet)
;(enable-window-numbering)
(enable-highlight-symbol)
(enable-auto-pair)
(enable-auto-complete)
(enable-auto-complete-c-header)
(enable-auto-complete-clang-async)
(enable-mproj)
;(enable-cscope)
;(enable-smart-compile)
;(enable-ztree)
;(enable-clang-format)
(enable-google-c-style)
(enable-shell-switcher)
;(enable-rtags)
(enable-session)
;(enable-ibuffer)
;(enable-browse-kill-ring)
(enable-tabbar)
;(enable-shell-pop)
;(enable-desktop)
;(enable-protobuf)
;(enable-multi-term)
(enable-auto-complete-java)
;; --------------------------- plugins feathers ----------------------------

