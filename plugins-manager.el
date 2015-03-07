
(defun enable-auto-complete ()
;  (require 'pos-tip)
  (require 'auto-complete-config)
  ;; solve conflicting with yas plugin
  (defun ac-yasnippet-candidates ()
	(with-no-warnings
	  (apply 'append (mapcar 'ac-yasnippet-candidates-1 (yas/get-snippet-tables)))
	  ))
  (add-to-list 'ac-dictionary-directories (concat emacs-relevance-path "/ac-dict"))
  (ac-config-default)
  ;; quick help
  (setq ac-auto-show-menu 0.8)
  (setq ac-use-quick-help t)
  (setq ac-quick-help-delay 0.5)
  (setq ac-fuzzy-enable t)

  (set-face-background 'ac-candidate-face "#366060")
  (set-face-foreground 'ac-selection-face "#1f1f1f")
  (set-face-background 'ac-selection-face "#8cd0d3")
  (set-face-foreground 'ac-selection-face "#1f1f1f")

  (setq-default ac-sources '(ac-source-yasnippet
							 ac-source-filename
							 ac-source-words-in-all-buffer
							 ac-source-functions
							 ac-source-variables
							 ac-source-symbols
							 ac-source-features
							 ac-source-abbrev
							 ac-source-words-in-same-mode-buffers
							 ac-source-dictionary))

  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t)
)

(defun enable-auto-complete-clang-async ()
;  (interactive)
  ;; just enable auto-complete-clang-async, is will config
  ;; when c/c++ loaded by my mproj
  (require 'auto-complete-clang-async)
  (defun my-ac-config-clang-async() 
	(setq ac-clang-complete-executable (concat emacs-bin-path "/clang-complete"))
	(ac-clang-launch-completion-process)
;	(add-hook 'after-save-hook  (lambda ()
;				(ac-clang-syntax-check)))
	)
  (add-hook 'c-mode-common-hook 'my-ac-config-clang-async)


;	(add-hook 'ede-minor-mode-hook (lambda ()
;									 (let* ((include-paths (ede-all-include-path)))
;									   (when (> (list-length include-paths) 0)
;										 (setq ac-clang-cflags (mapcar (lambda (path) (concat "-I" path)) include-paths))
;										 (ac-clang-update-cmdlineargs))))
)

(defun enable-yasnippet ()
  (require 'yasnippet)
  (yas-global-mode 1)
;  (add-hook 'prog-mode-hook 
;			'(lambda ()
;			   (yas-minor-mode)))
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

(defun enable-auto-complete-c-header ()
  (require 'auto-complete-c-headers)
)

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

(defun enable-google-c-style ()
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
)

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
		rtags-find-file-prefer-exact-match t)
;  (add-hook 'c-mode-common-hook 'rtags-start-process-maybe)
)

(defun enable-session ()
  (require 'session)
  (add-hook 'after-init-hook 'session-initialize)
)

(defun enable-ibuffer ()
  (require 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  )

(defun enable-browse-kill-ring ()
  (require 'browse-kill-ring)
  (global-set-key [(control c)(k)] 'browse-kill-ring)
  (browse-kill-ring-default-keybindings)
  )

(defun enable-desktop ()
  (require 'desktop)
  (desktop-save-mode 1)
)

(defun custom-tabbar-face ()
  ;; 设置tabbar外观  
  ;; 设置默认主题: 字体, 背景和前景颜色，大小  
)

(defun enable-tabbar ()
  (require 'tabbar)
  (tabbar-mode)
  (global-set-key (kbd "<M-up>")    'tabbar-backward-group)  
  (global-set-key (kbd "<M-down>")  'tabbar-forward-group)  
  (global-set-key (kbd "<M-left>")  'tabbar-backward-tab)
  (global-set-key (kbd "<M-right>") 'tabbar-forward-tab)

  (set-face-attribute 'tabbar-default-face nil  
					  :family "DejaVu Sans Mono"  
					  :background "gray80"  
					  :foreground "gray30"  
					  )  
  (set-face-attribute 'tabbar-button-face nil  
					  :box '(:line-width 1 :color "yellow")  
					  )  
  (set-face-attribute 'tabbar-selected-face nil  
					  :foreground "DarkGreen"  
					  :background "LightGoldenrod"  
					  :box '(:line-width 2 :color "DarkGoldenrod")  
					  :overline "black"  
					  :underline "black"  
					  :weight 'bold  
					  )  
  (set-face-attribute 'tabbar-unselected-face nil  
					  :box '(:line-width 2 :color "#00B2BF")  
					  )
  )

(defun enable-protobuf ()
  (require 'protobuf-mode)
  (setq auto-mode-alist  (cons '(".proto$" . protobuf-mode) auto-mode-alist))

  (defconst my-protobuf-style
	'((c-basic-offset . 2)
	  (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
			(lambda () (c-add-style "my-style" my-protobuf-style t)))
)

(defun enable-shell-pop ()
  (require 'shell-pop)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(shell-pop-default-directory "~/.elisp/shell-pop")
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-term-shell "/bin/bash")
   '(shell-pop-universal-key "C-t")
   '(shell-pop-window-height 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom"))
  )

(defun enable-multi-term ()
  (require 'multi-term)
  (setq multi-term-program "/bin/bash")
  (custom-set-variables
   '(term-default-bg-color "#000000")        ;; background color (black)
   '(term-default-fg-color "#dddd00"))       ;; foreground color (yellow)
)

(defun enable-mproj ()
  (require 'mproj)
  (add-hook 'c++-mode-hook 'mproj:apply-proj)
  (add-hook 'c-mode-hook 'mproj:apply-proj)
)

(provide 'plugins-manager)


