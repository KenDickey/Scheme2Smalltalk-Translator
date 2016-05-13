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
(SECTION 6 1)
(test #f not #t)
(test #f not 3)
(test #f not (list 3))
(test #t not #f)
(test #f not '())
(test #f not (list))
(test #f not 'nil)

(test #t boolean? #f)
(test #f boolean? 0)
(test #f boolean? '())
(SECTION 6 2)
(test #t eqv? 'a 'a)
(test #f eqv? 'a 'b)
(test #t eqv? 2 2)
(test #t eqv? '() '())
(test #t eqv? '10000 '10000)
(test #f eqv? (cons 1 2)(cons 1 2))
(test #f eqv? (lambda () 1) (lambda () 2))
(test #f eqv? #f 'nil)
(let ((p (lambda (x) x)))
  (test #t eqv? p p))
(define gen-counter
 (lambda ()
   (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(let ((g (gen-counter))) (test #t eqv? g g))
(test #f eqv? (gen-counter) (gen-counter))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (test #f eqv? f g))

(test #t eq? 'a 'a)
(test #f eq? (list 'a) (list 'a))
(test #t eq? '() '())
(test #t eq? car car)
(let ((x '(a))) (test #t eq? x x))
(let ((x '#())) (test #t eq? x x))
(let ((x (lambda (x) x))) (test #t eq? x x))

(test #t equal? 'a 'a)
(test #t equal? '(a) '(a))
(test #t equal? '(a (b) c) '(a (b) c))
(test #t equal? "abc" "abc")
(test #t equal? 2 2)
(test #t equal? (make-vector 5 'a) (make-vector 5 'a))

(SECTION 6 3)
(test '(a b c d e) 'dot '(a . (b . (c . (d . (e . ()))))))
(define x (list 'a 'b 'c))
(define y x)
(and list? (test #t list? y))
(set-cdr! x 4)
(test '(a . 4) 'set-cdr! x)
(test #t eqv? x y)
(test '(a b c . d) 'dot '(a . (b . (c . d))))
(and list? (test #f list? y))
(and list? (let ((x (list 'a))) (set-cdr! x x) (test #f 'list? (list? x))))

(test #t pair? '(a . b))
(test #t pair? '(a . 1))
(test #t pair? '(a b c))
(test #f pair? '())
(test #f pair? '#(a b))

(test '(a) cons 'a '())
(test '((a) b c d) cons '(a) '(b c d))
(test '("a" b c) cons "a" '(b c))
(test '(a . 3) cons 'a 3)
(test '((a b) . c) cons '(a b) 'c)

(test 'a car '(a b c))
(test '(a) car '((a) b c d))
(test 1 car '(1 . 2))

(test '(b c d) cdr '((a) b c d))
(test 2 cdr '(1 . 2))

(test '(a 7 c) list 'a (+ 3 4) 'c)
(test '() list)

(test 3 length '(a b c))
(test 3 length '(a (b) (c d e)))
(test 0 length '())

(test '(x y) append '(x) '(y))
(test '(a b c d) append '(a) '(b c d))
(test '(a (b) (c)) append '(a (b)) '((c)))
(test '() append)
(test '(a b c . d) append '(a b) '(c . d))
(test 'a append '() 'a)

(test '(c b a) reverse '(a b c))
(test '((e (f)) d (b c) a) reverse '(a (b c) d (e (f))))

(test 'c list-ref '(a b c d) 2)

(test '(a b c) memq 'a '(a b c))
(test '(b c) memq 'b '(a b c))
(test '#f memq 'a '(b c d))
(test '#f memq (list 'a) '(b (a) c))
(test '((a) c) member (list 'a) '(b (a) c))
(test '(101 102) memv 101 '(100 101 102))

(define e '((a 1) (b 2) (c 3)))
(test '(a 1) assq 'a e)
(test '(b 2) assq 'b e)
(test #f assq 'd e)
(test #f assq (list 'a) '(((a)) ((b)) ((c))))
(test '((a)) assoc (list 'a) '(((a)) ((b)) ((c))))
(test '(5 7) assv 5 '((2 3) (5 7) (11 13)))

(report-errs)
(newline)
"last item in file"
