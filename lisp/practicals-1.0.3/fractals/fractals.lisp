(in-package :com.philou.fractals)

(defun loose= (x y)
  (< (abs (- x y)) 1e-10))

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defun mpoint (x y)
  (make-instance 'point :x x :y y))

(defun point= (A B)
  (or (and (null A) (null B))
  (and
   (not (null A))
   (not (null B))
   (loose= (x A) (x B))
   (loose= (y A) (y B)))))

(deftest test-point= ()
  (check
   (point= nil nil)
   (not (point= nil (mpoint 1 0 )))
   (not (point= (mpoint 1 0) nil))
   (point= (mpoint 1 0) (mpoint 1 0 ))
   (not (point= (mpoint 1 0) (mpoint 2 0 )))
   (not (point= (mpoint 1 0) (mpoint 1 4 )))
   (point= (mpoint 0.5L0 0) (mpoint (/ 1 2) 0))))



(defclass line ()
  ((start-point :initarg :start-point :initform (make-instance 'point) :accessor start-point)
   (end-point :initarg :end-point :initform (make-instance 'point :x 1.0) :accessor end-point)))

(defun mline (start-point end-point)
  (make-instance 'line :start-point start-point :end-point end-point))

(defun line= (l1 l2)
  (or
   (and (null l1) (null l2))
   (and
    (not (null l1))
    (not (null l2))
    (point= (start-point l1) (start-point l2))
    (point= (end-point l1) (end-point l2)))))

(deftest test-line= ()
  (check
   (line= nil nil)
   (not (line= nil (mline (mpoint 1 0) (mpoint 2 5))))
   (not (line= (mline (mpoint 1 0) (mpoint 2 5)) nil))
   (line= (mline (mpoint 1 0) (mpoint 2 5)) (mline (mpoint 1 0 ) (mpoint 2 5)))
   (not (line= (mline (mpoint 1 0) (mpoint 2 5)) (mline (mpoint 1 5 ) (mpoint 2 5))))
   (not (line= (mline (mpoint 1 0) (mpoint 2 5)) (mline (mpoint 1 0 ) (mpoint 1 5))))))

(defun mapcar-closed-intervals (function list)
  (assert (>= (length list) 2))
  (let ((result ())
		 (previous (first list)))
    (dolist (current (rest list))
      (push (funcall function previous current) result)
      (setf previous current))
    (push (funcall function previous (first list)) result)
    (nreverse result)))

(defun mapcar-opened-intervals (function list)
  (mapcar function list (rest list)))


(deftest test-mapcar-closed-intervals ()
  (check
    (tree-equal '((a b) (b a)) (mapcar-closed-intervals #'list '(a b)))))

(deftest test-mapcar-opened-intervals ()
  (check
    (tree-equal '((a b) (b c)) (mapcar-opened-intervals #'list '(a b c)))))

(defun mlines (points)
  (cond
    ((null points) nil)
    ((null (rest points)) nil)
    (t (mapcar-opened-intervals #'mline points))))


(deftest test-mlines ()
  (check
    (null (mlines nil))
    (null (mlines `( ,(mpoint 0 0))))
    (tree-equal 
     (mlines (list (mpoint 0 0) (mpoint 1 1)))
     (list (mline (mpoint 0 0) (mpoint 1 1)))
     :test 'line=)
    (tree-equal 
     (mlines (list (mpoint 0 0) (mpoint 1 1) (mpoint 1 0)))
     (list (mline (mpoint 0 0) (mpoint 1 1)) (mline (mpoint 1 1) (mpoint 1 0)))
     :test 'line=)))



(defun write-svg-file (pathname lines)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream "<?xml version=\"1.0\" standalone=\"no\"?>~%")
    (format stream "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\" \"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">~%")
    (format stream "<svg width=\"24cm\" height=\"12cm\" viewBox=\"0 0 1000 500\" xmlns=\"http://www.w3.org/2000/svg\">~%")
    (format stream "  <g stroke=\"green\" stroke-width=\"2\">~%")
    (dolist (line lines)
      (format stream "    <line x1=\"~5$\" y1=\"~5$\" x2=\"~5$\" y2=\"~5$\"/>~%" (x (start-point line)) (y (start-point line)) (x (end-point line)) (y (end-point line))))
    (format stream "  </g>~%")
    (format stream "</svg>~%")))

(defun rotate (point &key (around (mpoint 0 0)) (angle (/ pi 3)))
  (with-slots ((x0 x) (y0 y)) around
    (with-slots (x y) point
      (mpoint 
       (+ x0 (- (* (cos angle) (- x x0)) (* (sin angle) (- y y0))))
       (+ y0 (+ (* (cos angle) (- y y0)) (* (sin angle) (- x x0))))))))

(deftest test-rotate ()
  (let ((angle (/ pi 3)))
    (check
     (point= (mpoint (cos angle) (sin angle)) (rotate (mpoint 1 0)))
     (point= 
      (mpoint (+ 1 (cos angle)) (+ 1 (sin angle)))
      (rotate (mpoint 2 1) :around (mpoint 1 1))))))

(defun barycentre (start-point end-point ratio)
  (with-slots ((x0 x) (y0 y)) start-point
    (with-slots (x y) end-point
      (mpoint (+ x0 (* ratio (- x x0))) (+ y0 (* ratio (- y y0)))))))

(deftest test-barycentre ()
  (check
   (point= (mpoint 1 1) (barycentre (mpoint 0 0) (mpoint 2 2) 0.5))
   (point= (mpoint 1 1) (barycentre (mpoint 0 0) (mpoint 3 3) (/ 1 3)))))

(defun break-up-line (line)
  (with-slots (start-point end-point) line
    (let* ((p1 (barycentre start-point end-point (/ 1 3)))
	   (p3 (barycentre start-point end-point (/ 2 3)))
	   (p2 (rotate p3 :around p1)))
      (mlines (list start-point p1 p2 p3 end-point)))))

(defun random-between (x1 x2)
  (if (> x1 x2)
      (random-between x2 x1)
      (+ x1 (random (- x2 x1)))))

(defun random-break-up (line)
  (with-slots (start-point end-point) line
    (let* ((middle (mpoint (random-between (x start-point) (x end-point))
				  (random-between (y start-point) (y end-point)))))
      (mlines (list start-point middle end-point)))))


(defun break-up-line-2 (line)
  (with-slots (start-point end-point) line
    (let* ((p1 (barycentre start-point end-point (/ 1 3)))
	   (p4 (barycentre start-point end-point (/ 2 3)))
	   (p2 (rotate p4 :around p1 :angle (/ pi 2)))
	   (p3 (rotate p1 :around p4 :angle (- 0 (/ pi 2)))))
      (mlines (list start-point p1 p2 p3 p4 end-point)))))

(defun recurs (states-list recurs-func times)
  (if (<= times 0)
      states-list
      (recurs (mapcan recurs-func states-list) recurs-func (- times 1))))
 
(deftest test-recurs ()
  (flet ((duplicate (x) (list x x)))
    (check
      (tree-equal '(a) (recurs '(a) #'duplicate 0))
      (tree-equal '(a a) (recurs '(a) #'duplicate 1))
      (tree-equal '(a a a a) (recurs '(a) #'duplicate 2)))))

(defun van-coch(start-point end-point break-up-line-func times)
  (recurs (list (mline start-point end-point)) break-up-line-func times))

(defun write-test-svg-file ( &key (times 3) (break-up-line-func #'break-up-line))
  (write-svg-file 
   #p"/home/poulet/lisp/practicals-1.0.3/fractals/test.svg"
   (van-coch (mpoint 0 0) (mpoint 1000 0) break-up-line-func times )))


