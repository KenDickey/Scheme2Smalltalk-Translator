;;;; `test.scm' Test correctness of scheme implementations.
;;; Copyright (C) 1991, 1992, 1993, 1994, 1995 Aubrey Jaffer.

;;; This includes examples from
;;; William Clinger and Jonathan Rees, editors.
;;; Revised^4 Report on the Algorithmic Language Scheme
;;; and the IEEE specification.

;;; The input tests read this file expecting it to be named "test.scm".
;;; Files `tmp1', `tmp2' and `tmp3' will be created in the course of running
;;; these tests.  You may need to delete them in order to run
;;; "test.scm" more than once.

;;;   There are three optional tests:
;;; (TEST-CONT) tests multiple returns from call-with-current-continuation
;;; 
;;; (TEST-SC4) tests procedures required by R4RS but not by IEEE
;;; 
;;; (TEST-DELAY) tests DELAY and FORCE, which are not required by
;;;   either standard.

;;; If you are testing a R3RS version which does not have `list?' do:
;;; (define list? #f)

;;; send corrections or additions to jaffer@ai.mit.edu or
;;; Aubrey Jaffer, 84 Pleasant St., Wakefield MA 01880, USA

(define cur-section '())(define errs '())
(define SECTION (lambda args
		  (display "SECTION") (write args) (newline)
		  (set! cur-section args) #t))
(define record-error (lambda (e) (set! errs (cons (list cur-section e) errs))))

(define test
  (lambda (expect fun . args)
    (write (cons fun args))
    (display "  ==> ")
    ((lambda (res)
      (write res)
      (newline)
      (cond ((not (equal? expect res))
	     (record-error (list res expect (cons fun args)))
	     (display " BUT EXPECTED ")
	     (write expect)
	     (newline)
	     #f)
	    (else #t)))
     (if (procedure? fun) (apply fun args) (car args)))))

(define (report-errs)
  (newline)
  (if (null? errs) (display "Passed all tests")
      (begin
	(display "errors were:")
	(newline)
	(display "(SECTION (got expected (call)))")
	(newline)
	(for-each (lambda (l) (write l) (newline))
		  errs)))
  (newline))

;;=====================;;
(SECTION 4 2 3)
(define x 0)
(test 6 'begin (begin (set! x 5) (+ x 1)))
(SECTION 4 2 4)
(test '#(0 1 2 3 4) 'do (do ((vec (make-vector 5))
			    (i 0 (+ i 1)))
			   ((= i 5) vec)
			 (vector-set! vec i i)))
(test 25 'do (let ((x '(1 3 5 7 9)))
	       (do ((x x (cdr x))
		    (sum 0 (+ sum (car x))))
		   ((null? x) sum))))
(test 1 'let (let foo () 1))
(test '((6 1 3) (-5 -2)) 'let
      (let loop ((numbers '(3 -2 1 6 -5))
		 (nonneg '())
		 (neg '()))
	(cond ((null? numbers) (list nonneg neg))
	      ((negative? (car numbers))
	       (loop (cdr numbers)
		     nonneg
		     (cons (car numbers) neg)))
	      (else
	       (loop (cdr numbers)
		     (cons (car numbers) nonneg)
		     neg)))))
(SECTION 4 2 6)
(test '(list 3 4) 'quasiquote `(list ,(+ 1 2) 4))
(test '(list a (quote a)) 'quasiquote (let ((name 'a)) `(list ,name ',name)))
(test '(a 3 4 5 6 b) 'quasiquote `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(test '((foo 7) . cons)
	'quasiquote
	`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))

;;; sqt is defined here because not all implementations are required to
;;; support it. 
(define (sqt x)
	(do ((i 0 (+ i 1)))
	    ((> (* i i) x) (- i 1))))

(test '#(10 5 2 4 3 8) 'quasiquote `#(10 5 ,(sqt 4) ,@(map sqt '(16 9)) 8))
(test 5 'quasiquote `,(+ 2 3))
(test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
      'quasiquote `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(test '(a `(b ,x ,'y d) e) 'quasiquote
	(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)))
(test '(list 3 4) 'quasiquote (quasiquote (list (unquote (+ 1 2)) 4)))
(test '`(list ,(+ 1 2) 4) 'quasiquote '(quasiquote (list (unquote (+ 1 2)) 4)))
(SECTION 5 2 1)
(define add3 (lambda (x) (+ x 3)))
(test 6 'define (add3 3))
(define first car)
(test 1 'define (first '(1 2)))
(SECTION 5 2 2)
(test 45 'define
	(let ((x 5))
		(define foo (lambda (y) (bar x y)))
		(define bar (lambda (a b) (+ (* a b) a)))
		(foo (+ x 3))))
(define x 34)
(define (foo) (define x 5) x)
(test 5 foo)
(test 34 'define x)
(define foo (lambda () (define x 5) x))
(test 5 foo)
(test 34 'define x)
(define (foo x) ((lambda () (define x 5) x)) x)
(test 88 foo 88)
(test 4 foo 4)
(test 34 'define x)

(report-errs)
(newline)
"last item in file"
