(progn
  (format t "~s" *load-truename*)
  (load (compile-file "/mnt/windows/Downloads/lispbox-0.7/practicals-1.0.3/Chapter08/load.lisp"))
  (load (compile-file "/mnt/windows/Downloads/lispbox-0.7/practicals-1.0.3/Chapter24/packages.lisp"))
  (load (compile-file "/mnt/windows/Downloads/lispbox-0.7/practicals-1.0.3/Chapter24/my-binary-data.lisp")))
