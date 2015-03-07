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

(defun directory-subdir-recursive (directory)
  "List directory path in DIRECTORY and is all sub directory."
  (let* ((paths-list '())
		 (current-directory-list (directory-files directory t)))
	(while current-directory-list
	  (let* ((f (car current-directory-list))
             (fn (file-name-base f)))
        (when (and 
               (not (string-equal ".." (substring f -2)))
               (not (string-equal "." (substring f -1)))
               (not (string-equal "." (substring fn 0 1)))
               (file-directory-p f)
               (file-readable-p f)
               )
          (setq paths-list (append paths-list (directory-subdir-recursive f)))
          (setq paths-list (cons f paths-list))
          )
        )
      (setq current-directory-list (cdr current-directory-list)))
	paths-list
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

(provide 'mutils)
