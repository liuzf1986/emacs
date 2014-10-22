;; 负责对c/c++ 头文件处理，包括cscope, ac/clang, auto header

(require 'utils)  ;; 获取gcc本地头文件列表
(require 'xcscope)
(require 'auto-complete)
(require 'auto-complete-clang-async)
(require 'auto-complete-c-headers)

(defcustom hds/lib-name-map
  (list
   '("local-c" utils/localc-get-inc)
   '("ndk" ("/root/software/android-ndk-r9d/platforms/android-18/arch-arm/usr/include/"))
   '("qt" ("")))
  "备选项目类型"
  :type '(list)
  :group 'hds)

(defcustom hds/cscope-tags-name 
  "cscope.out"
  "cscope数据库默认名称"
  :type '(string)
  :group 'hds)

(defcustom hds/cscope-src-list
  "cscope.files"
  "生成source列表文件的文件名"
  :type '(string)
  :group 'hds)

(defcustom hds/cscope-tags-root
  "~/.elisp/relevance/cscope-db"
  "cscope数据库存放根目录，各个库以字目录存在"
  :type '(string)
  :group 'hds)

(defun hds/get-lib-names ()
  "获取库类型列表"
  (let* ((maps hds/lib-name-map)
		 (lib-names ()))
	(while maps
	  (add-to-list 'lib-names (car (car maps)))
	  (setq maps (cdr maps))
	  )
	lib-names
	)
)

(defun hds/get-lib-incs (lib-name)
  "获取制定库的头文件列表"
  (let* ((header-desc (car (cdr (assoc lib-name hds/lib-name-map)))))
	(if (functionp header-desc)
		(funcall header-desc)
	  header-desc
	  )
	)
)

(defun hds/cscope-get-path (lib-name)
  "获取库头文件cscope数据库路径"
  (utils/populate-path hds/cscope-tags-root lib-name t)
)

(defun hds/cscope-get-db-path (lib-name)
  "获取库文件默认cscope数据库全路径"
  (concat (hds/cscope-get-path lib-name) hds/cscope-tags-name)
)

(defun hds/cscope-generate-db (src-folders dst-path)
  "根据src-folders生成cscope数据库，若制定的目录已经存在cscope数据库，删除。
src-folders为包含源代码的目录列表，也可以为单个目录的字符串形式
dst-path指定生成数据库将存放的目录，若为目录，则使用默认数据库名字；若为具体的名字，则按制定名字生成"
  (unless (or (stringp src-folders) (consp src-folders)) (error "指定的源代码列表非数组形式"))
  (unless (stringp dst-path) (error "制定的目标路径非字符串"))

  (let* ((src-list (if (consp src-folders)
					   src-folders
					 (cons src-folders ())))
		 (tag-folder (if (string= "" (file-name-nondirectory dst-path)) ; 目录
						 dst-path
					   (file-name-directory dst-path)))
		 (tag-name (if (string= "" (file-name-nondirectory dst-path)) ; 目录
					   hds/cscope-tags-name
					 (file-name-nondirectory dst-path)))
		 (src-list-path (concat tag-folder hds/cscope-src-list))
		 )
	;; 创建目录
	(mkdir tag-folder t)
	;; 删除搜索列表
	(if (file-exists-p src-list-path)
		(delete-file src-list-path))

	;; 遍历头文件列表，生成列表文件
	(while src-list
	  (let  ((incdir (car src-list)))
		(shell-command (concat "find " incdir " -type f -iregex \'.*\\.\\(c\\|cpp\\|h\\|java\\)\' >> " src-list-path))
		)
	  (setq src-list (cdr src-list))
	  )

	;; 生成tag文件
	(shell-command (concat "cscope -bkq -i " src-list-path " -f " (concat tag-folder tag-name)))
	(delete-file src-list-path)
	)
)

(defun hds/cscope-force-generate (lib-name)
  "强制生成库的cscope数据库"
    ;; 获取头文件路径
  (let* ((tagpath (hds/cscope-get-path lib-name))
		 (header-paths (hds/get-lib-incs lib-name)))

	(unless (consp header-paths) (error "获取库头文件结果非列表"))
	(hds/cscope-generate-db header-paths tagpath)
	)
  )

(defun hds/cscope-generate-lib (lib-name)
  "生成gcc编译环境所使用的头文件索引，若文件已经存在，不做任何操作"
  (interactive
   (list 
	(completing-read "选择库文件名字 : " (hds/get-lib-names))))

  (if (file-exists-p (hds/cscope-get-path lib-name))
	  (if (yes-or-no-p "所选库文件cscope数据库已经存在，是否重新生成? ")
		  (hds/cscope-force-generate lib-name)
		(minibuffer-message "操作取消")
	  )
	(hds/cscope-force-generate lib-name)
	)
)

(defun hds/cscope-update-proj (&optional basedir)
  "以当前工作目录为根目录，生成cscope索引文件"
  (interactive "D root path is : ")

  (let* ((src-path (if basedir
					   basedir
					 (get-current-path)))
		 )
	(setq src-path (if (utils-find-file-upward hds/cscope-tags-name src-path)
					   (file-name-directory (utils-find-file-upward hds/cscope-tags-name src-path))
					 src-path))
	(hds/cscope-generate-db (file-name-as-directory src-path) src-path)
	)
)

(defun hds/achead-append (paths)
  "添加头文件目录列表或头文件字符串"
  (let* ((path-list (if (consp paths)
			paths
		      (cons paths ())))
	 )
	(while path-list
	  (setq path-list (cdr path-list)))
	)
)


(defun hds/cscope-set-regex (root cscope-dbs)
  "设置xcscope的cscope-database-regexps变量，全部为优先搜索选项
root为项目根目录，headers为该项目引用的cscope数据库"
  (let* (
		 ; (root-regrex (format "^%s" root))
		 (root-regrex ".*")
		 (dbs-path cscope-dbs)
		 (cscope-regrex (list '(t) root-regrex)))
	(while dbs-path
	  (setq cscope-regrex (cons (cons (car dbs-path) ()) cscope-regrex))
	  (setq dbs-path (cdr dbs-path)))

	(setq cscope-database-regexps
		  (cons (reverse cscope-regrex) ())
		  )
	)
)

(provide 'hds)
