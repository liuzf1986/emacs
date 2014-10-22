
(defun enable-auto-complete ()
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat emacs-relevance-path "/ac-dict"))
  (ac-config-default)
  (setq ac-auto-start t)
)

(defun enable-auto-complete-clang-async ()
  (interactive)
  (require 'auto-complete-clang-async)
  (defun ac-cc-mode-setup ()
	(setq ac-clang-complete-executable (concat emacs-bin-path "/clang-complete"))
	(setq ac-sources '(ac-source-clang-async))
	(ac-clang-launch-completion-process)
	)
  (defun my-ac-config ()
	(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
	(add-hook 'ede-minor-mode-hook (lambda ()
									 (let* ((include-paths (ede-all-include-path)))
									   (when (> (list-length include-paths) 0)
										 (setq ac-clang-cflags (mapcar (lambda (path) (concat "-I" path)) include-paths))
										 (ac-clang-update-cmdlineargs)))))
	(add-hook 'auto-complete-mode-hook 'ac-common-setup)
	(global-auto-complete-mode t))
  (my-ac-config)
)

(defun enable-yasnippet ()
  (require 'yasnippet)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 
			'(lambda ()
			   (yas-minor-mode)))
)

(defun enable-ibus ()
  (require 'ibus)
  (add-hook 'after-init-hook 'ibus-mode-on)
)

(defun enable-color-theme ()
  (require 'color-theme)
  (load-file (concat emacs-relevance-path "/color-theme-sunburst.el"))
  (color-theme-tm)
)

(defun enable-sr-speed-bar ()
  (require 'sr-speedbar)
  (setq speedbar-use-images nil)
  ;;	  (with-current-buffer sr-speedbar-buffer-name
  ;;		(setq window-size-fixed 'width))
)

(defun enable-dirtree ()
  (autoload 'dirtree "dirtree" "Add directory to tree view " t))

(defun enable-auto-pair ()
  (require 'autopair)
  (autopair-global-mode))

;(defun achead:get-include-directories-function ()
;  (let* ((command-result (shell-command-to-string "echo \"\" | g++ -v -x c++ -E -"))
;         (start-string "#include <...> search starts here:\n")
;         (end-string "End of search list.\n")
;         (start-pos (string-match start-string command-result))
;         (end-pos (string-match end-string command-result))
;         (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
;    (split-string include-string))
(defun ac-c-headers-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-hook 'ede-minor-mode-hook (lambda ()
								   (setq achead:get-include-directories-function 'ede-all-include-path))))

(defun enable-auto-complete-c-header ()
  (add-hook 'c++-mode-hook 'ac-c-headers-init)
  (add-hook 'c-mode-hook 'ac-c-headers-init))

(defun enable-cscope ()
  (require 'xcscope)
    (setq cscope-do-not-update-database t)
	(setq cscope-database-regexps
		  '(
			(".*"
			 (t)
			 ("/home/liuzf1986/.elisp/relevance/cscope-db/stdc/")))))

(defun enable-smart-compile ()
  (require 'smart-compile))

(defun enable-window-numbering ()
  (require 'window-numbering)
  (window-numbering-mode t))

(defun enable-highlight-symbol ()
  (require 'highlight-symbol)
  (global-set-key [(control f3)] 'highlight-symbol-at-point)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(defun enable-ztree ()
  (require 'ztree-dir)
  (require 'ztree-diff))


(defun enable-clang-format ()
  (require 'clang-format))

(defun enable-bash-completion ()
  (autoload 'bash-completion-dynamic-complete 
	"bash-completion"
	"BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
			'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
			'bash-completion-dynamic-complete))

(defun enable-shell-switcher ()
  (require 'shell-switcher)
  (setq shell-switcher-mode t))


(defun enable-rtags ()
  (require 'rtags)
  (rtags-enable-standard-keybindings c-mode-base-map)
  (setq rtags-find-file-case-insensitive t
		rtags-find-file-prefer-exact-match t))

(provide 'plugins-manager)
