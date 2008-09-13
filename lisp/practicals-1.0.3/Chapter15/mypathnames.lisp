(defun component-present-p (value)
  (and
   value
   (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
   (not (pathname-name p))
   (not (pathname-type p))
   p))

(defun faulty-full-directory (pathname)
  (append (pathname-directory pathname)
	  (file-namestring pathname)))

(defun full-directory (pathname)
  (append (or (pathname-directory pathname) (list :relative))
	  (list (file-namestring pathname))))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p pathname))
	(make-pathname
	 :directory (full-directory pathname)
	 :type nil
	 :name nil
	 :defaults pathname)
	pathname)))


(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+opencml
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-are-files nil)))
    
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-directories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun file-exists (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathanme)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))

  #-(or sbcl lispworks openmcl allegro cmu clisp)
  (error "file-exists not implemented"))

(defun pathname-as-file (name)
  (let ((pathname (make-pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	      (name-and-type (first (last directory))))
	  (make-pathname
	   :name name-and-type
	   :directory (butlast directory)
	   :type name-and-type
	   :defaults pathname)
	pathname))))

(defun directory-p (pathname)
  (not (pathname-name (file-exists pathname))))

(defun file-p (pathname)
  (not (eql nil (pathname-name (file-exists pathname)))))
	    

(defun walk-directories (dirname fn &key directories (test (constantly t)))
  (let ((dirpath (pathname dirname)))
      (when (and (or directories
		     (file-p dirpath))
		 (funcall test dirpath))
	(funcall fn dirpath))
      (when (directory-p dirpath)
        (dolist (sub-path (list-directory dirpath))
	  (walk-directories sub-path fn :directories directories :test test)))))

