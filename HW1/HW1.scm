;;1.2
;;a. 10500900
;;b. 2.5e-007
;;c. big-number
;;d. cat
;;e. cheshire
;;f. 10500900
;;g. big-number
;;h. number1

;;1.3
;;a. 4
;;b. 2/5
;;c. 2/3
;;d. 0.6666666666666667

;;1.4
;;a. (- (* 4 7) (+ 13 5))
;;b. (* 3 (+ 4 (- -5 -3)))
;;c. (/ 2.5 (* 5 (/ 1 10)))
;;d. (* 5 (+ (* 537 (+ 98.3 (- 375 (* 2.5 153)))) 255))

;;1.5(alpha is a, beta is b, gamma is g)
;;a. a + ((b + g) - a)
;;b. (a * b) + (g * b)
;;c. (a - b) / (a - g)

;;1.6
;;a. (cons 'one (cons 'two (cons 'three (cons 'four '()))))
;;b. (cons 'one (cons (cons 'two (cons 'three (cons 'four '()))) '()))
;;c. (cons 'one (cons (cons 'two (cons 'three '())) (cons 'four '())))
;;d. (cons (cons 'one (cons 'two '())) (cons (cons 'three (cons 'four '())) '()))
;;e. (cons (cons (cons 'one '()) '()) '())

;;1.10
;;a. #f
;;b. #t
;;c. #f
;;d. #t

;;1.14
;;a. #t
;;b. #f
;;c. #f
;;d. #t
;;e. #f
;;f. #t

;;2.1
(define (second ls)
  (cadr ls))

;;2.3
;;(1 2)
;;((a b) (e f))

;;2.4
(define (juggle ls)
  (list (cadr ls) (caddr ls) (car ls)))

;;2.6
;;a. #t 
;;b. #t
;;c. #t
;;d. #f

;;2.7
;;a. #t  
;;b. #f 
;;c. #t
;;d. #f

;;2.10
(define last-item
  (lambda (ls)
    (if (null? (cdr ls))
	(car ls)
	(last-item (cdr ls)))))

(define member?
  (lambda (item ls)
    (if (null? ls)
	#f
        (or (equal? (car ls) item) (member? item (cdr ls)))))))

(define remove-1st
  (lambda (item ls)
    (if (null? ls)
	'()
	(if (equal? (car ls) item)
	    (cdr ls)
	    (cons (car ls) (remove-1st item (cdr ls)))))))

(define mystery
  (lambda (ls)
    (if (null? (cddr ls))
	(cons (car ls) '())
	(cons (car ls) (mystery (cdr ls))))))

;;2.12
;; Value is (1 2 3 4).
;; Recursive program that deletes the last value of a list.
;; Suitable name: remove-last

;;2.13
(define (subst-1st new old ls)
  (cond
   ((null? ls) '())
   ((equal? old (car ls)) (cons new (cdr ls)))
   (else (cons (car ls) (subst-1st new old (cdr ls))))))

;;2.14
(define insert-left-1st
  (lambda (new old ls)
    (cond
     ((null? ls) '())
     ((equal? (car ls) old)
      (cons new (cons old (cdr ls))))
     (else (cons (car ls)(insert-left-1st new old (cdr ls)))))))

;;2.15
(define (list-of-first-items ls)
  (if (null? ls) '()
      (cons (caar ls) (list-of-first-items (cdr ls)))))

;;2.16
(define (replace new ls)
  (if (null? ls) '()
      (cons new (replace new (cdr ls)))))

;;2.18
(define (remove-last item ls)
  (reverse (remove-1st item (reverse ls))))

