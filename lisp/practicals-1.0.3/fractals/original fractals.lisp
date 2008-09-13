(in-package :com.philou.fractals)

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defun mpoint (x y)
  (make-instance 'point :x x :y y))

(defun point= (A B)
  (and
   (= (x A) (x B))
   (= (y A) (y B))))

(deftest test-point= ()
  (check
    (point= (mpoint 1 0) (mpoint 1 0 ))
    (not (point= (mpoint 1 0) (mpoint 2 0 )))
    (not (point= (mpoint 1 0) (mpoint 1 4 )))))


(defclass line ()
  ((A :initarg :A :initform (make-instance 'point) :accessor A)
   (B :initarg :B :initform (make-instance 'point :x 1.0) :accessor B)))

(defun mline (A B)
  (make-instance 'line :A A :B B))

(defun line= (l1 l2)
  (and
   (point= (A l1) (A l2))
   (point= (B l1) (B l2))))

(deftest test-line= ()
  (check
    (line= (mline (mpoint 1 0) (mpoint 2 5)) (mline (mpoint 1 0 ) (mpoint 2 5)))
    (not (line= (mline (mpoint 1 0) (mpoint 2 5)) (mline (mpoint 1 5 ) (mpoint 2 5))))
    (not (line= (mline (mpoint 1 0) (mpoint 2 5)) (mline (mpoint 1 0 ) (mpoint 1 5))))))

(defun make-test-lines ()
  (list
   (mline (mpoint 100 300) (mpoint 300 100))
   (mline (mpoint 200 300) (mpoint 400 100))
   (mline (mpoint 300 300) (mpoint 500 100)))

(defun map-2 (function list)
  (assert (>= (length list) 2))
  (let ((previous (first list)))
    (dolist (current (rest list))
      (funcall function previous current)
      (setf previous current))
    (funcall function previous (first list))))

(deftest test-map-2 ()
  (let ((result nil))
    (map-2 #'(lambda (predecessor current)
	       (push (list predecessor current) result))
	    '(a b))
    (check
      (tree-equal '((b a) (a b)) result))))

(defun mlines (points)
  (cond
    ((null points) nil)
    ((null (rest points)) nil)
    ((null (rest (rest points))) (list (mline (first points) (second points))))
    (t (let ((result nil))
	 (map-2 points #'(lambda (previous current)
			   (push (mline previous current) result)))
	 (nreverse result)))))


(deftest test-mlines ()
  (check
    (null (mlines nil))
    (null (mlines `( ,(mpoint 0 0))))
    (tree-equal 
     (mlines `( ,(mpoint 0 0) ,(mpoint 1 1))) 
     `( ,(mline (mpoint 0 0) (mpoint 1 1)))
     :test 'line=)
    (tree-equal 
     (mlines `( ,(mpoint 0 0) ,(mpoint 1 1) ,(mpoint 1 0)))
     `( ,(mline (mpoint 0 0) (mpoint 1 1)) ,(mline (mpoint 1 1) (mpoint 1 0)) ,(mline (mpoint 1 0) (mpoint 0 0)))
     :test 'line=)))



(defun write-dumb-svg-file (pathname lines)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream "<?xml version=\"1.0\" standalone=\"no\"?>~%")
    (format stream "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\" \"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">~%")
    (format stream "<svg width=\"12cm\" height=\"12cm\" viewBox=\"0 0 1000 1000\" xmlns=\"http://www.w3.org/2000/svg\">~%")
    (format stream "  <g stroke=\"green\" stroke-width=\"2\">~%")
    (dolist (line lines)
      (format stream "    <line x1=\"~d\" y1=\"~d\" x2=\"~d\" y2=\"~d\"/>~%" (x (A line)) (y (A line)) (x (B line)) (y (B line))))
    (format stream "  </g>~%")
    (format stream "</svg>~%")))

(defun van-coch (depth))


;(deftest test-van-coch-0 ()
;  (let ((lines (van-coch 0)))))
	

