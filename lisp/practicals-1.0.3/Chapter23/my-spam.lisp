(in-package :philou.leloup.spam)

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)
(defparameter *corpus* (make-array 1000 :fill-pointer 0 :adjustable t))
(defparameter *max-chars* (* 10 1024))

(defun make-new-feature-database ()
  (make-hash-table :test #'equal))

(defvar *feature-database* (make-new-feature-database))
(defvar *total-spams* 0)
(defvar *total-hams* 0)
(defvar *results* ())


(defun classify (text)
  (classification (score (extract-features text))))

(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
  score))
  
(defclass word-feature ()
  ((word
    :documentation "The word this feature represents"
    :initarg :word
    :accessor word
    :initform (error "Must supply :word"))
   (spam-count
    :documentation "Number of spams we have seen this feature in."
    :initarg :spam-count
    :accessor spam-count
    :initform 0)
   (ham-count
    :documentation "Number of hams we have seen this feature in."
    :initarg :ham-count
    :accessor ham-count
    ::initform 0)))

(defun clear-database ()
  (setf 
   *feature-database* (make-new-feature-database)
   *total-hams* 0
   *total-spams* 0))
  
(defun intern-feature (word)
  "word-feature associated with word. By default, every word is associated an empty word-feature"
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
	    (make-instance 'word-feature :word word))))

(defun extract-words (text)
  "List containing the word in text, without duplication"
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  "List with the word features associated with the words in text "
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~a :hams ~d :spams ~d" word ham-count spam-count))))

(defun train (text type)
  "trains the filter with typical spam or ham messages"
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  "increments ham or spam count for a given word-feature"
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  "increments total ham or spam messages count"
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun spam-probability (feature)
  (with-slots (ham-count spam-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
	  (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional (assumed-probability 1/2) (weight 1))
  (let ((basic-probability (spam-probability feature))
	(data-points (+ (ham-count feature) (spam-count feature))))
    (/ (+ (* weight assumed-probability)
	  (* data-points basic-probability))
       (+ data-points weight))))

(defun score (features)
  (let ((spam-probs ())
	(ham-probs ())
	(number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
	(let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
	  (push spam-prob spam-probs)
	  (push (- 1.0d0 spam-prob) ham-probs)
	  (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
	  (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (ham-count spam-count) feature
    (and (zerop ham-count) (zerop spam-count))))

(defun fisher (probs number-of-probs)
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
	 for i below (/ degrees-of-freedom 2)
	 for prob = (exp (- m)) then (* prob (/ m i))
	 summing prob)
   1.0))

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defun add-directory-to-corpus (directory type corpus)
  (dolist (filename (list-directory directory))
    (add-file-to-corpus filename type corpus)))


(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
; theres a problem with this destructuring bind ...
	(destructuring-bind (file type) (aref corpus idx)
	  (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
	(destructuring-bind (file type) (aref corpus idx)
	  (multiple-value-bind (classification score)
	      (classify (start-of-file file *max-chars*))
	    (list
	     :file file
	     :type type
	     :classification classification
	     :score score)))))

(defun nshuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
	for other = (random (1+ idx))
	do (unless (= idx other)
	     (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (filename max-chars)
  (with-open-file (in filename)
    (let* ((length (min (file-length in) max-chars))
	   (text (make-string length))
	   (read (read-sequence text in)))
      (if (< read length)
	  (subseq text 0 read)
	  text))))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
	 (ham 'correct)
	 (spam 'false-positive)
	 (unsure 'missed-ham)))
      (spam
       (ecase classification
	 (ham 'false-negative)
	 (spam 'correct)
	 (unsure 'missed-spam))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive false-negative missed-ham missed-spam))
	 (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
	  for (label . count) in counts
	  do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
		     label count (* 100 (/ count total))))))

(defun test-classifier (corpus testing-fraction)
  (clear-database)
  (let ((shuffled (shuffle-vector corpus))
	(train-on (floor (* (length corpus) (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defun explain-classification (file)
  (let* ((text (start-of-file file *max-chars*))
	 (features (extract-features text))
	 (score (score features))
	 (classification (classification score)))
    (show-summary file text classification score)
    (dolist (feature (sorted-interesting features))
      (show-feature feature))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f~%" classification score))

(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))