;; 项目管理目标（主要针对c/c++项目，不加扩展考虑）
;; 根据不同项目类型，索引不同的ac-clang补全
;; 根据不同项目类型，索引不同cscope查询
;; 项目配置文件写入配置文件
;; 保存项目打开的文件，并有最大历史记录
;; 在项目根目录展示项目文件，可设置过滤
;; 智能编译(make, gcc, ndk-build)

;; 项目配置文件名字，存放在当前目录
;; 配置以二维数组方式存储，每个数组的第一项为key，持久化采用直接将object写入文件方式

; 所有头文件目录 auto-complete-c-headers
; cscope-database-regexps     xcscope
; achead:get-include-directories-function    auto-complete-c-headers
; ac-clang-flags  auto-complete-clang-async

(require 'hds)


(defconst proj/config-file ".eproj")
(defvar proj/configure 
  (list
   '("[proj-path]")
   '("[proj-type]")
   '("[proj-ext-inc]"))
)

(defvar proj/inc-list ())
(make-variable-buffer-local 'proj/configure)
(make-variable-buffer-local 'proj/inc-list)

(defun proj/get-proj-path ()
  "获取工程路径，分隔符结束"
  (if (cadr (assoc "[proj-path]" proj/configure)) 
	  (file-name-as-directory (file-truename (cadr (assoc "[proj-path]" proj/configure))))
	(progn
	  (minibuffer-message "工程目录未初始化")
	  nil)
	  )
)

(defun proj/create ()
  "创建工程"
  (let* ((proj-path (read-directory-name "工程根目录："))
		 (proj-types (completing-read "项目类型：" (hds/get-lib-names)))
		 (proj-ext-inc (read-directory-name "附加源代码路径："))
		 )
	(setcdr (assoc "[proj-path]" proj/configure) (cons proj-path ()))
	(setcdr (assoc "[proj-type]" proj/configure) (cons proj-types ()))
	(setcdr (assoc "[proj-ext-inc]" proj/configure) (cons proj-ext-inc ()))
	)
)

(defun proj/save ()
  "保存项目配置到配置文件，以分行的方式"
  (interactive)
  (let* ((tmp-config proj/configure)
		 (result-str ""))
	(while tmp-config 
	  (let* ((conf-item (car tmp-config))
			 (conf-tag (car conf-item))
			 (conf-param (cdr conf-item))
			 )
		(setq result-str (concat result-str conf-tag))
		(setq result-str (concat result-str "\n"))
		; 处理每个配置项参数列表
		(while conf-param
		  (setq result-str (concat result-str (car conf-param)))
		  (setq result-str (concat result-str "\n"))
		  (setq conf-param (cdr conf-param))
		  )
		)
	  (setq tmp-config (cdr tmp-config))
	  )

	(if (proj/get-proj-path) 
		(utils/save-string result-str (concat (proj/get-proj-path) proj/config-file)))
	)
)

(defun proj/proj-load ()
  "加载配置文件"
  (interactive)
  (let* ((conf-path (utils-find-file-upward proj/config-file)))	
	(if conf-path
		(progn
		  (let* ((scatter (utils/read-to-list conf-path "\n"))
				 (find-tag nil)
				 (find-params ()))
			(while scatter  ;; 遍历所有字符串行
			  (let* ((sym (car scatter)))
				(if (assoc sym proj/configure)
					(progn  ;; 找到一个标签
					  (if (and find-tag  find-params)
						  (setcdr (assoc find-tag proj/configure) (reverse find-params))
						  )
					  (setq find-tag sym)
					  (setq find-params ())
					  )
				  (setq find-params (cons sym find-params)) ;; 不是标签，视为上一个标签的属性
				  )
				)
			  (setq scatter (cdr scatter))
			  )

			(if (and find-tag  find-params)  ;; 遍历结束，加载最后的属性和参数
				(setcdr (assoc find-tag proj/configure) (reverse find-params))
			  )
			(minibuffer-message "完成加载项目配置！！！")
			t
			)
		  )
	  (progn
		(minibuffer-message "当前目录不受项目管理")
		nil
		)
	  )
	)
)

(defun proj/create-and-store ()
  "创建项目，并写入到项目配置文件"
  (interactive)
  (proj/create)
  (proj/save)
  (proj/apply)
  )

(defun proj/apply-achead (proj-root source-list)
  "配置auto-complete-c-headers"
  (let* ((acheader-list (cons (file-truename proj-root) ()))
		 (source-dirs (append acheader-list source-list))
		 )
	(setq achead:include-directories source-list)
	)
)

(defun proj/apply-acclang (proj-root source-list)
  "配置async-auto-complete-clang"
  (let* ((source-dirs source-list)
		 (ac-clang-spec-cflags (cons (concat "-I" (file-truename proj-root)) ()))
		 )
	(while source-dirs
	  (when (car source-dirs)
		(setq ac-clang-spec-cflags (cons (concat "-I" (file-truename (car source-dirs))) ac-clang-spec-cflags)))
	  (setq source-dirs (cdr source-dirs))
	  )
	(setq ac-clang-cflags ac-clang-spec-cflags)
	(ac-clang-update-cmdlineargs)
	)
)

(defun proj/apply ()
  "根据proj/configure配置xcscope,async-auto-complete-clang,auto-complete-c-headers等插件"
  (interactive)
  (let* ((source-list ())
		 (configure proj/configure)
		 (root-path (file-truename "./"))
		 )
	(while configure
	  (let* ((conf-item (car configure)) ;; 逐条解析配置
			 )
		(cond
		 ((and (string= "[proj-type]" (car conf-item)) (cdr conf-item)) ;; 项目类型配置项，从hds中取出头文件列表，并配置cscope数据库
		  (let ((types (cdr conf-item))
				(db-list ())
				(proj-root (cadr (assoc "[proj-path]" proj/configure))))
			(while types ;; 一个项目可以同时有多种类别
			  (setq source-list (append (hds/get-lib-incs (car types)) source-list)) ;; 增加源代码或头文件列表
			  (setq db-list (cons (hds/cscope-get-path (car types)) db-list)) ;; 增加cscope数据库配置项
			  ;; 循环
			  (setq types (cdr types)))
			
			(setq cscope-database-regexps 
				  (hds/cscope-set-regex proj-root db-list))
			(princ-list cscope-database-regexps)
			))
		 
		  ((and (string= "[proj-ext-inc]" (car conf-item)) (cdr conf-item))  ;; 制定附加头文件列表
		   (setq source-list (append (cdr conf-item) source-list))
		   )

		  ((and (string= "[proj-path]" (car conf-item)) (cdr conf-item)) ;; 获取工程根目录
		   (setq root-path (file-truename (cadr conf-item)))
		   )

		 )
		)

	  (setq configure (cdr configure))
	  )
	
	;; 配置文件解析完毕，将配置应用到工程
	(proj/apply-acclang root-path source-list)  ;; 配置async-auto-complete-clang
	(proj/apply-achead root-path source-list)  ;; 配置auto-complete-c-headers
	)
)

(defun proj/add-ext-inc (inc-dir)
  "添加附加头文件列表"
  (interactive "D 请输入附加头文件路径 : ")
  (if (and inc-dir (file-exists-p inc-dir))
	  (let* ((new-inc (file-name-as-directory inc-dir))
			 (ext-incs  (cdr (assoc "[proj-ext-inc]" proj/configure)))
			 )
		(unless ext-incs
		  (setq ext-incs ()))
		(setcdr (assoc "[proj-ext-inc]" proj/configure) (delete-dups (cons new-inc ext-incs)))
		(proj/save)
		)
	(minibuffer-message "请输入有效目录")
	)
)

(defun proj/apply-config ()
  "向上查找工程文件，找到则应用该配置"
  (interactive)
  (when (proj/proj-load)
	(proj/apply)
	)
)

(add-hook 'c-mode-common-hook 'proj/apply-config)
(provide 'proj)
