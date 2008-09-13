(define nil (list))

(define (reverse list)
  (define (reverse-iter list result)
    (if (null? list)
	result
	(reverse-iter (cdr list) (cons (car list) result))))
  (reverse-iter list nil))

(define (deep-reverse-2 list)
  (define (deep-reverse-iter list result)
    (cond ((null? list) result)
	  ((not (pair? list)) list)
	  (else (deep-reverse-iter (cdr list) (cons (deep-reverse (car list)) 
						    result)))))
  (deep-reverse-iter list nil))
      
(define (fringe tree)
  (define (fringe-iter tree result)
    (cond ((null? tree) result)
	  ((not (pair? tree)) (cons tree result))
	  (else (fringe-iter (car tree) (fringe-iter (cdr tree) result)))))
  (fringe-iter tree nil))


(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)

  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
(define (branch-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (is-mobile structure)
  (pair? structure))
(define (is-weight structure)
  (not (is-mobile structure)))

(define (total-weight structure)
  (if (is-weight structure)
      structure
      (+ (branch-structure (left-branch structure))
	 (branch-structure (rigth-branch structure)))))

(define (is-balanced structure)
  (if (is-weight structure)
      true
      (let ((left (left-branch structure))
	    (right (right-branch structure)))
	(and (= (branch-torque left) (branch-torque right))
	     (is-balanced (branch-structure left))
	     (is-balanced (branch-structure right))))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))
      
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate i j)
  (if (> i j)
      nil
      (cons i (enumerate (+ i 1) j))))

(define (true-for-all xs predicate)
  (accumulate (lambda (x res) (and res (predicate x))) true xs))

(define (exists xs predicate)
  (not (true-for-all xs (lambda (x) (not (predicate x))))))

(define (contains xs item)
  (exists xs (lambda (x) (= x item))))

(define (uniques xs)
  (if (null? list)
      true
      (and (not (contains (cdr xs) (car xs)))
	   (uniques (cdr xs)))))

(define (reverse-index xs)
  (cond ((null? xs) nil)
	((null? (cdr xs)) (list (cons 1 (car xs))))
	(else 
	 (let ((rest (reverse-index (cdr xs))))
	   (cons (cons (+ 1 (caar rest)) (car xs))
		 rest)))))

(define (are-on-diag pos-1 pos-2)
  (= (abs (- (car pos-2) (car pos-1)))
     (abs (- (cdr pos-2) (cdr pos-1)))))

(define (diag-safe? indexed-queens-pos)
  (if (null? indexed-queens-pos)
      true
      (and (diag-safe? (cdr indexed-queens-pos))
	   (not (exists (cdr indexed-queens-pos)
			(lambda (pos) (are-on-diag pos (car indexed-queens-pos))))))))

(define (adjoin-position rest-of-queens new-queen-pos)
  (cons new-queen-pos rest-of-queens))

(define (safe? queens-pos)
  (and (uniques queens-pos)
       (diag-safe (reverse-index queens-pos))))

(define (queens board-size)
  (define (queens-cols columns)
    (if (= 0 columns)
	(list nil)
	(flatmap (lambda (rest-of-queens) 
		   (filter safe? 
			   (map (lambda (pos)
				  (adjoin-position rest-of-queens pos))
				(enumerate 1 board-size))))
		 (queens-cols (- columns 1)))))
  (queens-cols board-size))	