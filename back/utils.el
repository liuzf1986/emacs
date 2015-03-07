(defun list-subfolders (parent)
  "获取某目录的所有一级子目录"
  (let* ((files-list '())
	 (current-directory-list
	  (directory-files parent t)))
    (while current-directory-list
      (let ((f (car current-directory-list)))
	(if (and f (file-directory-p f))
	    (setq files-list (cons f files-list))
	  )
        )
      (setq current-directory-list (cdr current-directory-list))
      )
    files-list
    )
)

(defun append-load-paths (paths)
  "批量将路径添加到load-path"
  (while paths
    (let ((f (car paths)))
      (add-to-list 'load-path f)
      (message "add to paths" f)
      )
    (setq paths (cdr paths))
    )
)

(defun append-subdir-to-load-path (folder)
  "将某目录的所有子目录都加入load-path"
  (let ((subdirs (list-subfolders folder)))
    (while subdirs
      (let ((subdir (car subdirs)))
	(add-to-list 'load-path subdir)
	)
      (setq subdirs (cdr subdirs))
      )
    )
  )

(defun get-current-path ()
  "获取当前目录"
  (file-name-directory (or load-file-name buffer-file-name))
)

(defun linux-fullscreen ()
  "linux平台全屏函数"
  (interactive)
  (if (display-graphic-p)
	  (progn
		(x-send-client-message
		 nil 0 nil "_NET_WM_STATE" 32
		 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
		)	
	(message "当前非graphic模式，无法全屏显示")
	)
)

(defun linux-maximize ()
  "linux平台最大化函数"
  (interactive)
  (if (display-graphic-p)
	  (progn
		(x-send-client-message
		 nil 0 nil "_NET_WM_STATE" 32
		 '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
		(x-send-client-message
		 nil 0 nil "_NET_WM_STATE" 32
		 '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
		)
	(message "当前非graphic模式，无法最大化")
	)
)


(defun utils/localc-get-inc ()
  "获取g++使用的头文件路径"
  (let* ((command-result (shell-command-to-string "echo \"\" | g++ -v -x c++ -E -"))
         (start-string "#include <...> search starts here:\n")
         (end-string "End of search list.\n")
         (start-pos (string-match start-string command-result))
         (end-pos (string-match end-string command-result))
         (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
    (split-string include-string)))

(defun utils/scscope-get-lib-db (arg)
  "获取库头文件cscope数据库路径"
  (file-name-as-directory (concat (file-name-as-directory  emacs-cscope-common-db) arg))
)

(defun utils-generate-cscope-db (&optional tagpath)
  "生成gcc编译环境所使用的头文件索引"
  (interactive "D 设置cscope数据库存放路径 : ")
  ;; 设置默认
  (unless tagpath
	(setq tagpath (utils/scscope-get-lib-db "stdcm"))
	)
  ;; 创建目录
  (mkdir tagpath t)
  ;; 获取头文件路径
  (let* ((header-paths (utils/localc-get-inc))
		 (listpath (concat tagpath ".lst")))
	;; 删除已有的搜索列表和tag文件
	(if (file-exists-p listpath)
		(delete-file listpath))
	(if (and (file-exists-p tagpath) (not (file-directory-p tagpath)))
		(delete-file tagpath))

	;; 遍历头文件列表，生成列表文件
	(while header-paths
	  (let  ((incdir (car header-paths)))
		(shell-command (concat "find " incdir " -type f -iregex \'.*\\.\\(c\\|cpp\\|h\\|java\\)\' >> " listpath))
		  )
	  (setq header-paths (cdr header-paths))
	  )

	;; 生成tag文件
	(shell-command (concat "cscope -bkq -i " listpath " -f " tagpath "cscope.out"))
	(delete-file listpath)
	)
)

(defun cscope-generate-spec (&optional basedir)
  "以当前工作目录为根目录，生成cscope索引文件"
  (interactive "D root path is : ")
  (unless basedir
	(setq basedir (get-current-path))
	)
  (let* ((root-path basedir)
		 (prj-path (file-name-directory (file-truename root-path)))
		 (lst-path (concat prj-path "cscope.files"))
		 (db-path (concat prj-path "cscope.out"))
		 )

	;; 生成列表文件
	(shell-command (concat "find " prj-path " -type f -iregex \'.*\\.\\(c\\|cpp\\|h\\|java\\)\' > " lst-path))
	;; 生成标签文件
	(shell-command (concat "cscope -bkq -i " lst-path " -f " db-path))
	(delete-file lst-path)
	)
)

(defun utils/populate-path (basedir sub is-folder)
  "组装成完整路径"
  (if is-folder
	  (concat (file-name-as-directory (file-truename basedir)) (file-name-as-directory sub))
	(concat (file-name-as-directory (file-truename basedir)) sub)
	  )
)

(defun utils/save-string (text filename)
  "将字符串内容保存到制定文件"
  (with-temp-buffer
    (insert text)
    (write-region (point-min) (point-max) filename nil))
)

(defun utils/read-to-list (filename splitor)
  "从文件中读取字符串，并分割为字符串数组"
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string
     (buffer-string) splitor t)
    ) 
)

(defun utils-find-file-upward (regrex &optional basedir)
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
		(utils-find-file-upward regrex (file-name-directory (directory-file-name current-dir)))
		)
	  )
	)
  )

(defun directory-files-recursive (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories.
Return files that match the regular expression MATCH but ignore
files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
		 (current-directory-list
		  (directory-files directory t)))
	;; while we are in the current directory
	(while current-directory-list
	  (let ((f (car current-directory-list)))
		(cond
		 ((and
		   ignore ;; make sure it is not nil
		   (string-match ignore f))
										; ignore
		  nil
		  )
		 ((and
		   (file-regular-p f)
		   (file-readable-p f)
		   (string-match match f))
		  (setq files-list (cons f files-list))
		  )
		 ((and
		   (file-directory-p f)
		   (file-readable-p f)
		   (not (string-equal ".." (substring f -2)))
		   (not (string-equal "." (substring f -1)))
		   (> maxdepth 0))
		  ;; recurse only if necessary
		  (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1) ignore)))
		  (setq files-list (cons f files-list))
		  )
		 (t)
		 )
		)
	  (setq current-directory-list (cdr current-directory-list))
	  )
	files-list
	)
  )

(provide 'utils)


(defun directory-files-recursive (directory match maxdepth ignore) ""
  (let* ((files-list '())
		 (current-directory-list
		  (directory-files directory t)))
	;; while we are in the current directory
	(while current-directory-list
	  (let ((f (car current-directory-list)))
		(cond
		 ((and
		   (file-directory-p f)
		   (file-readable-p f)
		   (not (string-equal ".." (substring f -2)))
		   (not (string-equal "." (substring f -1)))
		   (> maxdepth 0))
		  ;; recurse only if necessary
		  (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1) ignore)))
		  (setq files-list (cons f files-list))
		  )
		 (t)
		 )
		)
	  (setq current-directory-list (cdr current-directory-list))
	  )
	files-list
	)
  )
