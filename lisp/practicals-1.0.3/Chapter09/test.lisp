(in-package :com.gigamonkeys.test)

(defvar *test-name* nil)

(defvar *all-tests* (make-hash-table))

(defun remove-all-tests ()
  (clrhash *all-tests*))

(defmacro with-test-name (name &body body)
  `(let ((*test-name* (append *test-name* (list ,name))))
    ,@body))

(defun add-test(package name test-function)
  (if (null (gethash package *all-tests*))
      (setf (gethash package *all-tests*) (make-hash-table)))
  (setf (gethash name (gethash package *all-tests*)) test-function))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other
test functions or use `check' to run individual test cases."
  `(progn
    (defun ,name ,parameters
      (with-test-name ',name
	,@body))
    (add-test *package* ',name #',name)))

(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating `forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by `check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defun test-everything ()
  (maphash #'(lambda (package package-tests) (test-package package)) *all-tests*))

(defun test-package (package)
  (with-test-name (package-name package)
    (maphash #'(lambda (test-name test) (funcall test)) (gethash package *all-tests*))))

(defun test-current-package ()
  (test-package *package*))








(deftest my-test-list ()
  (check
   (= 3 3))
  (test-toto))

(deftest test-toto ()
  (check
   (= 1 1)
   (= 1 2)))

