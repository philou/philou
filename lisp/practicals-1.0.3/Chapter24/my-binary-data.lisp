(in-package :com.gigamonkeys.my-binary-data)

(defun binary-class-spec->symbol-properties (name superclasses slots)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get ',name 'slots) ',(mapcar #'first slots))
    (setf (get ',name 'superclasses) ',superclasses)))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
	nconc (direct-slots super)
	nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses) (mapcar #' first slots)))

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun define-binary-class-slot->defclass-slot (spec)
  (let ((name (first spec)))
  `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun binary-class-spec->defclass (name superclasses slots)
  `(defclass ,name ,superclasses
    ,(mapcar #'define-binary-class-slot->defclass-slot slots)))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type  stream value &key)
  (:documentation "Writes the value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Reads an object's state from the stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Writes an object's state to the stream."))

(defun list-if-atom (x)
  (if (listp x) x
      (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (list-if-atom (second spec))))

(defun slot->read-value(spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
   `(setf ,name (read-value ',type ,stream ,@args))))

(defun binary-class-spec->defmethod-read-object (name slots superclasses)
  (with-gensyms (objectvar streamvar)
    `(defmethod read-object progn ((,objectvar ,name) ,streamvar)
      (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots)))))

(defun slot->write-value(spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun binary-class-spec->defmethod-write-object (name slots superclasses)
  (with-gensyms (objectvar streamvar)
    `(defmethod write-object progn ((,objectvar ,name) ,streamvar)
      (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots)))))

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  `(progn
    ,(binary-class-spec->symbol-properties name superclasses slots)
    ,(binary-class-spec->defclass name superclasses slots)
    ,read-method
    ,(binary-class-spec->defmethod-write-object name slots superclasses)))

(defmacro define-binary-class (name (&rest superclasses) slots)
  `(define-generic-binary-class ,name ,superclasses ,slots
				,(binary-class-spec->defmethod-read-object name slots superclasses)))

;(define-binary-class id3-tag ()
;    ((file-identifier  (iso-8859-1-string :length 3))
;     (major-version    u1)
;     (revision         u1)
;     (flags            u1)
;     (size             id3-tag-size)
;     (frames           (id3-frames :tag-size size))))

(defmethod read-value ((type symbol) in &key)
  (let ((object (make-instance type)))
    (read-object object in)
    object))

(defmethod write-value ((type symbol) out value &key)
  (assert (typep value type))
  (write-object value out))

(defun slot->keyword-arg(spec)
  (list (as-keyword (first spec)) (first spec)))

(defun slot->binding(spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
   `(,name (read-value ',type ,stream ,@args))))

(defun tagged-binary-class-spec->defmethod-read-value (name slots options)
  (with-gensyms (typevar objectvar streamvar)
    `(defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
      (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
	(let ((,objectvar 
	       (make-instance 
		,@(or (rest (assoc :dispatch options))
		     (error "Must supplu :dispatch form."))
		,@(mapcan #'slot->keyword-arg slots))))
	  ,objectvar)))))

(defmacro define-tagged-binary-class (name superclasses slots &rest options)
  `(define-generic-binary-class ,name ,superclasses ,slots
				,(tagged-binary-class-spec->defmethod-read-value name slots options)))

;(define-tagged-binary-class id3-frame ()
;  ((id (iso-8859-1-string :length 3))
;   (size u3))
;  (:dispatch (find-frame-class id)))

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (typevar streamvar objectvar)
       (destructuring-bind (supertype &rest super-args) (list-if-atom (first spec))
			    `(progn
			      (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key ,@args)
				(read-value ',supertype ,streamvar ,@super-args))
			      (defmethod write-value ((,typevar (eql ',name)) ,streamvar ,objectvar &key ,@args)
				(write-value ',supertype ,streamvar ,objectvar ,@super-args))))))
    (2
     (with-gensyms (typevar)
       `(progn
	 ,(destructuring-bind ((invar) &body body) (rest (assoc :reader spec))
			      `(defmethod read-value ((,typevar (eql ',name)) ,invar &key ,@args)
				,@body))
	 ,(destructuring-bind ((outvar objectvar) &body body) (rest (assoc :writer spec))
			      `(defmethod write-value ((,typevar (eql ',name)) ,outvar ,objectvar &key ,@args)
				,@body)))))))

;(define-binary-type iso-8859-1-string (length)
;  (:reader (in)
;	   (let ((string (make-string length)))
;	     (dotimes (i length)
;	       (setf (char string i) (code-char (read-byte in))))
;	     string))
;  (:writer (out value)
;	   (dotimes (i length)
;	     (write-byte (char-code (char value i)) out))))



(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))