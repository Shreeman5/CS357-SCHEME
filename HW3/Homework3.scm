;; Name: Gautams, Shreeman
;; Net ID: gautams@unm.edu

;; === Part 1, Points: 10, Weight: 1/3 ===
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; problem1-1-7.2    p234
;; Points: 1/10
(define compose3
  (lambda (f g h)
    (lambda (x)
      ((compose f g) (h x)))))

;; problem1-1-7.3    p234
;; Points: 1/10
(define compose-many
  (lambda procedure
    (lambda (value)
      (letrec ((compose-many-helper
        (lambda (ls value)
          (if(null? ls)
            value
            (compose-many-helper (cdr ls) ((car ls) value))))))
	(compose-many-helper (reverse procedure) value)))))

;; problem1-1-7.6    p235
;; Points: 1/10
(define mf2
  (lambda (proc ls)
    (letrec ((mf2-h
      (lambda (input output)
	(if (null? input)
	  output
	(if (null? (cdr input))
	  output
	  (mf2-h (cdr input) (append output (list (proc (car input) (cadr input))))))))))   
      (mf2-h ls '()))))		

;; problem1-1-7.7    p235
;; Points: 1/10
(define reduce
  (lambda (proc ls)
    (letrec ((loop
	      (lambda (ls acc)
		(if (null? ls)
		    acc
		    (loop (cdr ls) (proc acc (car ls)))))))
      (loop (cddr ls) (proc (car ls) (cadr ls))))))


;; problem1-1-7.8    p236
;; Points: 1/10
(define andmap
  (lambda (pred ls)
    (letrec ((andmap-h
      (lambda (ls)
	(if (null? ls)
	  #t
	  (and (pred (car ls)) andmap-h(cdr ls))))))
      (andmap-h ls))))

;; problem1-1-7.12    p243
;; Points: 1/10
(define curried*
  (lambda (m)
    (lambda (n)
      (* m n))))

;;Currying call
(define times10
  (curried* 10))

;; problem1-1-7.18    p244
;; Points: 1/10
(define between?
  (lambda (x y z)
    (if (and (> y x) (> z y))
      #t
      #f)))

;;Curried version of between
(define between?-c
  (lambda (x)
    (lambda (y)
      (lambda (z)
	(if (and (> y x) (> z y))
	  #t
	  #f)))))

;; problem1-1-7.22    p250
;; Points: 1/10
(define mult-by-scalar
  (lambda (c)
    (flat-recur '() (lambda (x y) (cons (* c x) y)))))

;;Using flat-recur to multiply the list by a scalar value
(define flat-recur
  (lambda (seed list-proc)
    (letrec
      ((helper
        (lambda (ls)
	  (display ls) newline
      	  (if (null? ls)
	    seed
	    (list-proc (car ls) (helper (cdr ls)))))))
  helper)))

;; problem1-1-7.30    p
;; Points: 1/10

(define reverse-all
  (lambda ls
    (deep-recur '() (lambda (x y) (append y (list x))) (lambda (x y)(append y (list x))))))

;;Using deep-recur to reverse a list deeply
(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
      ((helper
        (lambda (ls)
          (if (null? ls)
            seed
            (let ((a (car ls)))
              (if (or (pair? a) (null? a))
                (list-proc (helper a) (helper (cdr ls)))
                (item-proc a (helper (cdr ls)))))))))
  helper)))

;; problem1-1-7.31    p
;; Points: 1/10



;; === Part 2, Points: 10, Weight: 1/3 ===

;; problem2-1-a
;; Points: 1/8
(define tail-recur
  (lambda (bpred xproc aproc acc0)
    (lambda (x)
      (letrec ((helper
        (lambda (x acc)
	  (if (bpred x)
	    acc
	    (helper (xproc x) (aproc x acc))))))
	(helper x acc0)))))

;; problem2-1-b
;; Points: 1/8
(define reverse
  (tail-recur null? cdr (lambda (x acc) (cons (car x) acc)) '()))

;; problem2-1-c
;; Points: 1/8
(define iota
  (tail-recur zero? sub1 cons '()))

;; problem2-2-
;; Points: 1/8
(define disjunction2
  (lambda (pred0 pred1)
    (lambda (x)
      (or (pred1 x) (pred0 x)))))

;; problem2-3-
;; Points: 1/8
(define disjunction
  (lambda ls
    (lambda (x)
      (letrec ((loop
		(lambda (ls)
		  (cond ((null? ls) #t)
			(((car ls) x) #t)
			(else (loop (cdr ls)))))))
	(loop ls)))))

;; problem2-4-
;; Points: 1/8
(define matrix-map
  (lambda (proc matrix)
    (let
      ((map-row
        (lambda (row)
          (map proc row))))
    (map map-row matrix))))

;; problem2-5
(define fold
  (lambda (seed proc)
    (letrec
      ((pattern
        (lambda (ls)
          (if (null? ls)
            seed
            (proc (car ls) (pattern (cdr ls)))))))
     pattern)))

;; problem2-5-a
;; Points: 1/8
(define delete-duplicates
  (fold '() (lambda (x y)(if (member x y) y (cons x y)))))

;; problem2-5-b
;; Points: 1/8
(define assoc
  (lambda (item ls)
    ((fold #f (lambda (x y) (if (equal? (car x) item) x y)))
     ls)))


;; === Part 3, Points: 8, Weight: 1/3 ===

;; problem3-1-
;; Points: 1/8
(define length
  (lambda (ls)
    (apply + (map (lambda (x) 1) ls))))

;; problem3-2-
;; Points: 1/8
(define sum-of-squares
  (lambda (ls)
    (apply + (map (lambda (x) (* x x)) ls))))

;; problem3-3-
;; Points: 1/8
(define avg
  (lambda (ls)
    (/ (apply + ls) (length ls))))

;; problem3-4-
;; Points: 1/8
(define avg-odd
  (lambda (ls)
    (avg (filter odd? ls))))

;; problem3-5-
;; Points: 1/8
(define shortest
  (lambda (ls)
    (if (null? ls)
      '()
      ((fold (car ls) (lambda (x y) (if (< (length x) (length y)) x y)))
  ls))))

;; problem3-6-
;; Points: 1/8
(define avg-fact
  (lambda (ls)
    (let ((fact (lambda (x) (apply * (iota x)))))
      (avg (map fact ls)))))

;; problem3-7-
;; Points: 1/8
(define tally
  (lambda (pred ls)
    (length (filter pred ls))))

;; problem3-8-
;; Points: 1/8

(define select
  (lambda (pred ls0 ls1)
    (cond ((null? ls0) '())
	  ((pred (car ls0))
	   (cons (car ls1)
		 (select pred (cdr ls0) (cdr ls1))))
	  (else
	   (select pred (cdr ls0) (cdr ls1))))))

(define list-ref
  (lambda (ls n)
    (select (lambda (x) (= x n))
	    (iota n)
	    ls)))

