;; Name: Gautam, Shreeman
;; Net ID: gautams@unm.edu

;; problem1-4.4    p129
;; Points: 1/15

(define deepen-1
  (lambda (ls)
    (if (null? ls) 
      '()
      (cons (list (car ls)) (deepen-1 (cdr ls))))))

;; problem1-4.6    p136
;; Points: 1/15

(define insert-left-all
  (lambda (new old ls)
    (cond
     ((null? ls) '())
      ((equal? (car ls) old) 
        (cons new (cons old (insert-left-all new old (cdr ls)))))
      ((pair? (car ls))
        (cons (insert-left-all new old (car ls))(insert-left-all new old (cdr ls))))
    (else
      (cons (car ls) (insert-left-all new old (cdr ls)))))))

;; problem1-4.10    p143
;; Points: 1/15

(define leftmost
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls))
        (leftmost (reverse (car ls))))
      (else
       (car ls)))))

(define rightmost
  (lambda (ls)
    (leftmost (reverse ls))))
     

;; problem1-4.11    p143
;; Points: 1/15

(define rightmost
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((and (pair? (car ls)) (null? (cdr ls)))
        (rightmost (car ls)))
      ((null? (cdr ls))
        (car ls))
      (else
        (rightmost (cdr ls))))))

;; problem1-4.18    p156
;; Points: 1/15

(define length-it
  (lambda (ls)
    (length-it-helper ls 0)))

;;Iterative helper that utilizes tail recursion to
;;compute the length of a list
(define length-it-helper
  (lambda (ls value)
    (if (null? ls)
      value
      (length-it-helper (cdr ls) (+ value 1)))))

;; problem1-4.19    p156
;; Points: 1/15

(define mk-asc-list-of-ints
  (lambda (n)
    (mk-asc-list-of-ints-helper n '())))

;;Iterative helper that utilizes tail recursion to
;;make an ascending list of numbers, starting at 1
;;and ending at n
(define mk-asc-list-of-ints-helper
  (lambda (n ls)
    (if (= n 0)
      ls
      (mk-asc-list-of-ints-helper (- n 1) (cons n ls)))))

;; Points: 1/15

(define mk-desc-list-of-ints
  (lambda (n)  
    (mk-desc-list-of-ints-helper 1 n '())))



;;Iterative helper that utilizes tail recursion to
;;make a descending list of numbers, starting at n
;;and ending at 1
(define mk-desc-list-of-ints-helper
  (lambda (counterVar n ls)
    (if (< n counterVar)
      ls
      (mk-desc-list-of-ints-helper (+ counterVar 1) n (cons counterVar ls)))))

;; problem1-4.20    p156
;; Points: 1/15

(define occurs 
  (lambda (checkItem ls)
    (cond
      ((null? ls) 0)
      ((equal? checkItem (car ls)) 
        (+ 1 (occurs checkItem (cdr ls))))
      (else
        (occurs checkItem (cdr ls))))))

;; Points: 1/15

(define occurs-it
  (lambda (checkItem ls)
    (occurs-iterative-helper checkItem 0 ls)))

;;
;;Iterative helper that utilizes tail recursion to
;;calculate the top-level occurences of an item in
;;a list
(define occurs-iterative-helper
  (lambda (checkItem counter ls)
    (cond
      ((null? ls) counter)
      ((equal? checkItem (car ls))
        (occurs-iterative-helper checkItem (+ counter 1) (cdr ls)))
      (else
        (occurs-iterative-helper checkItem counter (cdr ls))))))

;; problem2-
;; Points: 1/15

(define calculator
  (lambda (ls)
    (cond
      ((pair? ls)(calculate-recursive ls))
    (else
      ls))))

;;Helper function that recursively calculates
;;the value of an infix expression
(define calculate-recursive
  (lambda (ls)
    (if (equal? '+ (cadr ls))
      (cond
        ((and (not (number? (car ls))) (not (number? (caddr ls))))
	  (+ (calculate-recursive (car ls)) (calculate-recursive (caddr ls))))
        ((and (number? (car ls)) (not (number? (caddr ls))))
          (+ (car ls) (calculate-recursive (caddr ls))))
        ((and (not (number? (car ls))) (number? (caddr ls)))
	  (+ (calculate-recursive (car ls)) (caddr ls)))
        ((and (number? (car ls)) (number? (caddr ls)))
          (+ (car ls) (caddr ls)))) 
    (if (equal? '* (car (cdr ls)))
      (cond
	((and (not (number? (car ls))) (not (number? (caddr ls))))
          (* (calculate-recursive (car ls)) (calculate-recursive (caddr ls)))) 
        ((and (number? (car ls)) (not (number? (caddr ls))))
          (* (car ls) (calculate-recursive (caddr ls))))
        ((and (not (number? (car ls))) (number? (caddr ls)))
          (* (calculate-recursive (car ls)) (caddr ls)))
        ((and (number? (car ls)) (number? (caddr ls)))
          (* (car ls) (caddr ls))))
    (if (equal? '/ (cadr ls))
      (cond
        ((and (not (number? (car ls))) (not (number? (caddr ls))))
          (/ (calculate-recursive (car ls)) (calculate-recursive (caddr ls))))     
        ((and (number? (car ls)) (not (number? (caddr ls))))
          (/ (car ls) (calculate-recursive (caddr ls))))
        ((and (not (number? (car ls))) (number? (caddr ls)))
          (/ (calculate-recursive (car ls)) (caddr ls)))
        ((and (number? (car ls)) (number? (caddr ls)))
          (/ (car ls) (caddr ls))))
      (cond
        ((and (not (number? (car ls))) (not (number? (caddr ls))))
	  (- (calculate-recursive (car ls)) (calculate-recursive (caddr ls))))
        ((and (number? (car ls)) (not (number? (caddr ls))))
          (- (car ls) (calculate-recursive (caddr ls))))
        ((and (not (number? (car ls))) (number? (caddr ls)))
          (- (calculate-recursive (car ls)) (caddr ls)))
        ((and (number? (car ls)) (number? (caddr ls)))
	  (- (car ls) (caddr ls)))
	))))))

;; problem3-
;; Points: 1/15

(define infix->prefix
  (lambda (ls)
    (cond
      ((pair? ls)(infixRecurse ls))
    (else
       ls))))

;;Helper method that recursively changes
;;infix into prefix notation
(define infixRecurse
  (lambda (ls)
    (cond
      ((and (not (number? (car ls))) (not (number? (caddr ls))))
        (cons (cadr ls) (cons (infixRecurse (car ls)) (cons (infixRecurse (caddr ls)) '()))))
      ((and (number? (car ls)) (not (number? (caddr ls))))
        (cons (cadr ls) (cons (car ls) (cons (infixRecurse (caddr ls)) '()))))
      ((and (not (number? (car ls))) (number? (caddr ls)))
        (cons (cadr ls) (cons (infixRecurse (car ls)) (cons (caddr ls) '()))))
      ((and (number? (car ls)) (number? (caddr ls)))
        (cons (cadr ls) (cons (car ls) (cons (caddr ls) '()))))
      )))

;; problem4-
    ;; NOTE: All helper functions should be tail-recursive and should be defined within the body of iota-iota using letrec.
;; Points: 1/15

;;Helper function iota-iota-rec is
;;defined within the body of iota-iota,
;;using letrec, to give a list that returns
;;pairs from 1 to n, as specified by the
;;question
(define iota-iota
  (lambda (i)
    (letrec
      ((iota-iota-rec
        (lambda (xVal yVal ls)
          (cond
	    ((> yVal i)
	     (iota-iota-rec (+ 1 xVal) 1 ls))
            ((> xVal i)
              (reverse ls))
            ((<= yVal i)
              (iota-iota-rec xVal (+ 1 yVal) (cons (cons xVal (cons yVal '())) ls)))))))
      (iota-iota-rec 1 1 '()))))

;; problem5-
    ;; NOTE: Any helper functions you need should be defined within the body of digits->number using letrec.
;; Points: 1/15

;;Helper function digits->number-rec is
;;defined within the body of digits->number,
;;using letrec, to give a number representative
;;of the list in decimal form
(define digits->number
  (lambda (ls)
    (letrec
      ((digits->number-rec
        (lambda (checkVar ls lengthOfNumber total)
          (cond
            ((< checkVar lengthOfNumber)
              (digits->number-rec (+ checkVar 1) (cdr ls) lengthOfNumber (+ total (* (car ls) (expt 10 (- (- lengthOfNumber 1) checkVar))))))
          (else total)))))
      (digits->number-rec 0 ls (length ls) 0))))

;; problem6-
;; Points: 1/15



;; problem7-
    ;; NOTE: Do not use or define fact or expt, any helper functions you need should be defined within the body of cos using letrec
;; Points: 1/15

;;Helper function cos-rec is
;;defined within the body of cos,
;;using letrec, to give a number that is
;;obtained from the taylor series of x,
;;without using fact or expt, but rather,
;;using some mathematic principles
(define cos
  (lambda (x)
    (letrec
      ((cos-rec
        (lambda (total denominatorFact numeratorPower oddEven factorialCounter)
          (cond
            ((>= oddEven 100)
              total)
            ((eq? (modulo oddEven 2) 1)
              (cos-rec (- total (/ numeratorPower denominatorFact)) (* denominatorFact (* factorialCounter (+ 1 factorialCounter)))
                       (* numeratorPower (* x x)) (+ 1 oddEven) (+ 2 factorialCounter)))
            ((eq? (modulo oddEven 2) 0)
              (cos-rec (+ total (/ numeratorPower denominatorFact)) (* denominatorFact (* factorialCounter (+ 1 factorialCounter)))
                       (* numeratorPower (* x x)) (+ 1 oddEven) (+ 2 factorialCounter)))))))
      (cos-rec 0 1 1 0 1))))

(define occurs
  (lambda (item ls)
    (if (null? ls)
      0
    (if (equal? item (car ls))
      (+ 1 (occurs item (cdr ls)))
      (occurs item (cdr ls))))))

(define occurs-it
  (lambda (item ls)
    (occurs-it-helper 0 item ls)))

(define occurs-it-helper
  (lambda (index item ls)
    (cond
      ( (null? ls) index )
      ( (equal? item (car ls)) (occurs-it-helper (+ index 1) item (cdr ls)))
    (else
      ( occurs-it-helper index item (cdr ls))))))

(define iota-iota
  (lambda (n)
    (letrec
      ((iota-iota-helper
        (lambda (xVal yVal ls)
	  (if (> xVal n)
	    (reverse ls)
	  (if (<= yVal n)
	    (iota-iota-helper xVal (+ yVal 1) (cons (cons xVal (cons yVal '())) ls))
	    (iota-iota-helper (+ xVal 1) 1 ls)  
	)))))
      (iota-iota-helper 1 1 '()))))

(define digits->number
  (lambda (ls)
    (letrec
      ((digits->number-helper
        (lambda (value ls counter)
	  (if (null? ls)
	    value
	    (digits->number-helper (+ value (* (car ls) (expt 10 counter )) ) (cdr ls) (+ 1 counter))   
	))))
      (digits->number-helper 0 (reverse ls) 0))))

(define cos
  (lambda (x)
    (letrec
      ((cos-helper
	(lambda (numer denom total counter factCounter)
	  (if (>= counter 100)
	     total
	  (if (even? counter)
             (cos-helper (* numer (* x x)) ( * denom (* factCounter (factCounter + 1))) (+ total (/ numer denom)) (+ 1 counter) (+ 2 factCounter))
	  (if (odd? counter)
	     (cos-helper (* numer (* x x)) ( * denom (* factCounter (factCounter + 1))) (- total (/ numer denom)) (+ 1 counter) (+ 2 factCounter))
	))))))
      (cos-helper 1 1 0 0 0) )))

(define take
  (lambda (n ls)
    (if (= n 0)
        '()
        (cons (car ls)
              (take (- n 1) (cdr ls))))))

(define drop
  (lambda (n ls)
    (if (= n 0)
        ls
        (drop (- n 1) (cdr ls)))))

(define sublist
  (lambda (ls n m)
    (take (- m n) (drop n ls))))

(define append
  (lambda (ls0 ls1)
    (if (null? ls0)
        ls1
        (cons (car ls0)
              (append (cdr ls0) ls1)))))

(define reverse
  (lambda (ls)
    (if (null? ls)
        '()
        (append (reverse (cdr ls))
                (list (car ls))))))

(define reverse
  (lambda (ls)
    (letrec
      ((loop
        (lambda (ls acc)
          (if (null? ls)
              acc
              (loop (cdr ls) (cons (car ls) acc))))))
      (loop ls '()))))

(define append
  (lambda (ls0 ls1)
    (letrec
      ((loop
        (lambda (ls acc)
          (if (null? ls)
              acc
              (loop (cdr ls) (cons (car ls) acc))))))
      (loop (loop ls0 '()) ls1))))

(define deep-reverse
  (lambda (ls)
    (cond ((null? ls) '())
          ((pair? (car ls))
           (append (deep-reverse (cdr ls))
                   (list (deep-reverse (car ls)))))
          (else
           (append (deep-reverse (cdr ls))
                   (list (car ls)))))))


(define delete
  (lambda (item ls)
    (cond ((null? ls) '())
          ((eq? (car ls) item)
           (delete item (cdr ls)))
          (else
           (cons (car ls)
                 (delete item (cdr ls)))))))


(define delete-duplicates
  (lambda (ls)
    (if (null? ls)
        '()
        (let
	  ((first (car ls)))
            (cons first (delete first (delete-duplicates (cdr ls))))))))

(define vars
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (caar ls)
              (vars (cdr ls))))))

(define vals
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (cadar ls)
              (vals (cdr ls))))))

(define let->lambda
  (lambda (sexpr)
    (display sexpr)
    (newline)
    (let ((defs (cadr sexpr)))
      (append (list (list 'lambda (vars defs) (caddr sexpr)))
              (vals defs)))))

(define list?
  (lambda (sexpr)
    (or (null? sexpr)
	(and (pair? sexpr)
	     (list? (cdr sexpr))))))

(define deep-times
  (lambda (sexpr)
    (display sexpr)
    (newline)
    (cond ((null? sexpr) 1)
          ((pair? sexpr)
           (* (deep-times (car sexpr))
              (deep-times (cdr sexpr))))
          (else
           sexpr))))


(define deep-flatten
  (lambda (ls)
    (cond ((null? ls) '())
          ((pair? (car ls))
           (append (deep-flatten (car ls))
                   (deep-flatten (cdr ls))))
          (else
           (cons (car ls)
                 (deep-flatten (cdr ls)))))))


(define deepen-1
  (lambda (ls)
    (if (null? ls) 
      '()
      (cons (cons (car ls) '()) (deepen-1 (cdr ls))))))


(define reverse
  (lambda (ls)
    (if (null? ls)
      '()
      (append (reverse (cdr ls))(cons (car ls) '()) ))))

(define deep-reverse
  (lambda (ls)
    (if (null? ls)
      '()
    (if (pair? (car ls))
      (append (deep-reverse (cdr ls)) (list (deep-reverse (car ls))))
      (append (deep-reverse (cdr ls)) (cons (car ls) '()))))))	

(define deep-flatten
  (lambda (ls)
    (if (null? ls)
      '()
    (if (pair? (car ls))
      (append (deep-flatten (car ls)) (deep-flatten (cdr ls)))
      (cons (car ls) (deep-flatten (cdr ls)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
