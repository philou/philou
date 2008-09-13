(in-package :com.gigamonkeys.my-id3v2)

;; unsigned integers

(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
	   (loop with value = 0
		 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
		 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte in))
		 finally (return value)))
  (:writer (out value)
	   (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
		 (write-byte (ldb (byte bits-per-byte low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1 :bits-per-byte 8))
(define-binary-type u2 () (unsigned-integer :bytes 2 :bits-per-byte 8))
(define-binary-type u3 () (unsigned-integer :bytes 3 :bits-per-byte 8))
(define-binary-type u4 () (unsigned-integer :bytes 4 :bits-per-byte 8))

;; id3 tag size

(define-binary-type id3-tag-size () (unsigned-integer :bytes 4 :bits-per-byte 7))

;; strings

(define-binary-type generic-string (length character-type)
  (:reader (in)
	   (let ((value (make-string length)))
	     (dotimes (i length)
	       (setf (char value i) (read-value character-type in)))
	     value))
  (:writer (out value)
	   (dotimes (i length)
	     (write-value character-type out (char value i)))))

(define-binary-type generic-terminated-string (terminator character-type)
  (:reader (in)
	   (with-output-to-string (s)
	     (loop for char = (read-value character-type in)
		   until (char= char terminator) do
		   (write-char char s))))
  (:writer (out value)
	   (loop for char across value do
		 (write-value character-type out char)
		 finally (write-value character-type out terminator))))

(define-binary-type iso-8859-1-char ()
  (:reader (in)
	   (let ((code (read-byte in)))
	     (or (code-char code)
		 (error "Character code ~d not supported" code))))
  (:writer (out value)
	   (let ((code (char-code value)))
	     (if (<= 0 code #xff)
		 (write-byte code out)
		 (error "Illegal character for iso-8859-1 encoding: character: ~c with code ~d" value code)))))

(define-binary-type iso-8859-1-string (length)
  (generic-string :length length :character-type 'iso-8859-1-char))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (generic-terminated-string :terminator terminator :character-type 'iso-8859-1-char))

(define-binary-type ucs-2-char (swap)
  (:reader (in)
	   (let ((code (read-value 'u2 in)))
	     (when swap (setf code (swap-bytes code)))
	     (or (code-char code)
		 (error "Character code ~d not supported" code))))
  (:writer (out value)
	   (let ((code (char-code value)))
	     (unless (<= 0 code #xffff)
	       (error "Illegal character for ucs-2 encoding: character: ~c with code ~d" value code))
	     (when swap (setf code (swap-bytes code)))
	     (write-value 'u2 out code))))

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code)))

(define-binary-type ucs-2-char-big-endian () (ucs-2-char :swap nil))

(define-binary-type ucs-2-char-little-endian () (ucs-2-char :swap t))

(defun ucs-2-char-type (byte-order-mark)
  (ecase byte-order-mark
    (#xfeff 'ucs-2-char-big-endian)
    (#xfffe 'ucs-2-char-little-endian)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in))
		 (characters (1- (/ length 2))))
	     (read-value
	      'generic-string in
	      :length characters
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out value)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-string out value
	    :length length ;(length value);c'est ça dans le livre, mais ça semble faux
	    :character-type (ucs-2-char-type #xfeff))))


(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
	   (let ((byte-order-mark (read-value 'u2 in)))
	     (read-value
	      'generic-terminated-string in
	      :terminator terminator
	      :character-type (ucs-2-char-type byte-order-mark))))
  (:writer (out value)
	   (write-value 'u2 out #xfeff)
	   (write-value
	    'generic-terminated-string out value
	    :terminator terminator
	    :character-type (ucs-2-char-type #xfeff))))

(define-binary-class id3-tag ()
  ((identifier     (iso-8859-1-string :length 3))
   (major-version  u1)
   (revision       u1)
   (flags          u1)
   (size           id3-tag-size)
   (frames         (id3-frames :tag-size size))))

(defun read-id3 (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'id3-tag in)))

(defun show-tag-header (file)
  (with-slots (identifier major-version revision flags size) (read-id3 file)
    (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
	    identifier major-version revision flags size (enough-namestring file))))

(defun mp3-p (file)
  (and
   (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))

(defun show-tag-headers (dir)
  (walk-directory dir #'show-tag-header :test #'mp3-p))

(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4))))
    (flet ((count-versions (file)
	     (incf (cdr (assoc (major-version (read-id3 file)) versions)))))
      (walk-directory dir #'count-versions :test #'mp3-p))
    versions))

(defun id3-p (file)
  (with-open-file (in file)
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

(define-tagged-binary-class id3-frame ()
  ((id (iso-8859-1-string :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(define-binary-class generic-frame (id3-frame)
  ((data (raw-bytes :size size))))

(define-binary-type raw-bytes (size)
  (:reader (in)
	   (let ((buf (make-array size :element-type '(unsigned-byte 8))))
	     (read-sequence buf in)
	     buf))
  (:writer (out value)
	   (write-sequence value out :end size)))

(defun find-frame-class (id)
  (declare (ignore id))
  'generic-frame)

(define-binary-type id3-frame (tag-size)
  (:reader (in)
	   (loop with to-read = tag-size
		 while (plusp to-read)
		 for frame = (read-frame in)
		 while frame
		 do (decf to-read (+ 6 (size frame)))
		 collect frame
		 finally (loop repeat (1- to-read) do (read-byte in))))
  (:writer (out value)
	   (loop with to-write = tag-size
		 for frame in value
		 do (write-value 'id3-frame out frame)
		 (decf to-write (+ 6 (size frame)))
		 finally (loop repeat to-write do (write-byte 0 out)))))