(setf asdf:*central-registry*
  '(*default-pathname-defaults*
    #p"~/lisp/practicals-1.0.3/fractals/"
    #p"~/lisp/practicals-1.0.3/Chapter03/"
    #p"~/lisp/practicals-1.0.3/Chapter08/"
    #p"~/lisp/practicals-1.0.3/Chapter09/"
    #p"~/lisp/practicals-1.0.3/Chapter24/"
    #p"~/lisp/practicals-1.0.3/Chapter25/"))

(setf asdf:*central-registry*
  '(*default-pathname-defaults*
    #p"/home/poulet/lisp/systems/"))

(asdf:operate 'asdf:load-op 'my-id3v2)

(asdf:operate 'asdf:load-op 'fractals)

(asdf:operate 'asdf:load-op 'chapter-3)




