(in-package :cl-user)

(let ((cl-macro-utilities-load-directory
        (make-pathname :name nil :type nil :version nil
                       :defaults (parse-namestring *load-truename*))))
  (load (compile-file (make-pathname :name "packages" :type "lisp" :defaults cl-macro-utilities-load-directory)))
  (load (compile-file (make-pathname :name "macro-utilities" :type "lisp" :defaults cl-macro-utilities-load-directory))))
