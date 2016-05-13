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

;;=======================;;
(SECTION 6 8)
(test #t vector? '#(0 (2 2 2 2) "Anna"))
(test #t vector? '#())
(test '#(a b c) vector 'a 'b 'c)
(test '#() vector)
(test 3 vector-length '#(0 (2 2 2 2) "Anna"))
(test 0 vector-length '#())
(test 8 vector-ref '#(1 1 2 3 5 8 13 21) 5)
(test '#(0 ("Sue" "Sue") "Anna") 'vector-set
	(let ((vec (vector 0 '(2 2 2 2) "Anna")))
	  (vector-set! vec 1 '("Sue" "Sue"))
	  vec))
(test '#(hi hi) make-vector 2 'hi)
(test '#() make-vector 0)
(test '#() make-vector 0 'a)
(SECTION 6 9)
(test #t procedure? car)
(test #f procedure? 'car)
(test #t procedure? (lambda (x) (* x x)))
(test #f procedure? '(lambda (x) (* x x)))
(test #t call-with-current-continuation procedure?)
(test 7 apply + (list 3 4))
(test 7 apply (lambda (a b) (+ a b)) (list 3 4))
(test 17 apply + 10 (list 3 4))
(test '() apply list '())
(define compose (lambda (f g) (lambda args (f (apply g args)))))

(define (sqt x)
	(do ((i 0 (+ i 1)))
	    ((> (* i i) x) (- i 1))))

(test 30 (compose sqt *) 12 75)
(define (sqt x) (do ((i 0 (+ i 1))) ((> (* i i) x) (- i 1))))
(test '(b e h) map cadr '((a b) (d e) (g h)))
(test '(5 7 9) map + '(1 2 3) '(4 5 6))
(test '#(0 1 4 9 16) 'for-each
	(let ((v (make-vector 5)))
		(for-each (lambda (i) (vector-set! v i (* i i)))
			'(0 1 2 3 4))
		v))
(test -3 call-with-current-continuation
		(lambda (exit)
		 (for-each (lambda (x) (if (negative? x) (exit x)))
		 	'(54 0 37 -3 245 19))
		#t))
(define list-length
  (lambda (obj)
   (call-with-current-continuation
    (lambda (return)
     (letrec ((r (lambda (obj) (cond ((null? obj) 0)
 				((pair? obj) (+ (r (cdr obj)) 1))
 				(else (return #f))))))
 	(r obj))))))
(test 4 list-length '(1 2 3 4))
(test #f list-length '(a b . c))
(test '() map cadr '())

;;; This tests full conformance of call-with-current-continuation.  It
;;; is a separate test because some schemes do not support call/cc
;;; other than escape procedures.  I am indebted to
;;; raja@copper.ucs.indiana.edu (Raja Sooriamurthi) for fixing this
;;; code.  The function leaf-eq? compares the leaves of 2 arbitrary
;;; trees constructed of conses.  
(define (next-leaf-generator obj eot)
   (letrec ((return #f)
 	   (cont (lambda (x)
 		   (recur obj)
 		   (set! cont (lambda (x) (return eot)))
 		   (cont #f)))
 	   (recur (lambda (obj)
 		      (if (pair? obj)
			  (for-each recur obj)
 			  (call-with-current-continuation
 			   (lambda (c)
			     (set! cont c)
 			     (return obj)))))))
     (lambda () (call-with-current-continuation
 		(lambda (ret) (set! return ret) (cont #f))))))
(define (leaf-eq? x y)
   (let* ((eot (list 'eot))
 	 (xf (next-leaf-generator x eot))
 	 (yf (next-leaf-generator y eot)))
     (letrec ((loop (lambda (x y)
 		     (cond ((not (eq? x y)) #f)
 			   ((eq? eot x) #t)
 			   (else (loop (xf) (yf)))))))
       (loop (xf) (yf)))))
(define (test-cont)
   (newline)
   (display ";testing continuations; ")
   (newline)
   (SECTION 6 9)
   (test #t leaf-eq? '(a (b (c))) '((a) b c))
   (test #f leaf-eq? '(a (b (c))) '((a) b c d))
   (report-errs))

;;; Test Optional R4RS DELAY syntax and FORCE procedure
(define (test-delay)
  (newline)
  (display ";testing DELAY and FORCE; ")
  (newline)
  (SECTION 6 9)
  (test 3 'delay (force (delay (+ 1 2))))
  (test '(3 3) 'delay (let ((p (delay (+ 1 2))))
			(list (force p) (force p))))
  (test 2 'delay (letrec ((a-stream
			   (letrec ((next (lambda (n)
					    (cons n (delay (next (+ n 1)))))))
			     (next 0)))
			  (head car)
			  (tail (lambda (stream) (force (cdr stream)))))
		   (head (tail (tail a-stream)))))
  (letrec ((count 0)
	   (p (delay (begin (set! count (+ count 1))
			    (if (> count x)
				count
				(force p)))))
	   (x 5))
    (test 6 force p)
    (set! x 10)
    (test 6 force p))
  (test 3 'force
	(letrec ((p (delay (if c 3 (begin (set! c #t) (+ (force p) 1)))))
		 (c #f))
	  (force p)))
  (report-errs))

(SECTION 6 10 1)
(test #t input-port?  (current-input-port))
(test #t output-port? (current-output-port))
(test #t call-with-input-file "test.scm" input-port?)
(define this-file (open-input-file "test.scm"))
(test #t input-port? this-file)
(SECTION 6 10 2)
(test #\; peek-char this-file)
(test #\; read-char this-file)
(test '(define cur-section '()) read this-file)
(test #\( peek-char this-file)
(test '(define errs '()) read this-file)
(close-input-port this-file)
(close-input-port this-file)
(define (check-test-file name)
   (define test-file (open-input-file name))
   (test #t 'input-port?
 	(call-with-input-file
 	    name
 	  (lambda (test-file)
 	    (test load-test-obj read test-file)
 	    (test #t eof-object? (peek-char test-file))
 	    (test #t eof-object? (read-char test-file))
 	    (input-port? test-file))))
  (test #\; read-char test-file)
  (test display-test-obj read test-file)
  (test load-test-obj read test-file)
  (close-input-port test-file))
(SECTION 6 10 3)
(define write-test-obj
  '(#t #f #\a () 9739 -3 . #((test) "te \" \" st" "" test #() b c)))
(define display-test-obj
  '(#t #f a () 9739 -3 . #((test) te " " st test #() b c)))
(define load-test-obj
  (list 'define 'foo (list 'quote write-test-obj)))
(test #t call-with-output-file
      "tmp1"
      (lambda (test-file)
	(write-char #\; test-file)
	(display write-test-obj test-file)
	(newline test-file)
	(write load-test-obj test-file)
	(output-port? test-file)))
(check-test-file "tmp1")

(define test-file (open-output-file "tmp2"))
(write-char #\; test-file)
(display write-test-obj test-file)
(newline test-file)
(write load-test-obj test-file)
(test #t output-port? test-file)
(close-output-port test-file)
(check-test-file "tmp2")
(define (test-sc4)
  (newline)
  (display ";testing scheme 4 functions; ")
  (newline)
  (SECTION 6 7)
  (test '(#\P #\space #\l) string->list "P l")
  (test '() string->list "")
  (test "1\\\"" list->string '(#\1 #\\ #\"))
  (test "" list->string '())
  (SECTION 6 8)
  (test '(dah dah didah) vector->list '#(dah dah didah))
  (test '() vector->list '#())
  (test '#(dididit dah) list->vector '(dididit dah))
  (test '#() list->vector '())
  (SECTION 6 10 4)
  (load "tmp1")
  (test write-test-obj 'load foo)
  (report-errs))

(report-errs)

(if (and (string->number "0.0") (inexact? (string->number "0.0")))
    (test-inexact))

(let ((n (string->number "281474976710655")))
  (if (and n (exact? n))
      (test-bignum)))
(newline)
(display "To fully test continuations, Scheme 4, and DELAY/FORCE do:")
(newline)
(display "(test-cont) (test-sc4) (test-delay)")
(test-sc4)   ;; @@ KenD
(test-delay) ;; @@ KenD
(test-cont)  ;; @@ KenD

(newline)

"last item in file"
