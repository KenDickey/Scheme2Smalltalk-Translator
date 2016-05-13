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
(SECTION 6 5 5)
(test #t number? 3)
(test #t complex? 3)
(test #t real? 3)
(test #t rational? 3)
(test #t integer? 3)

(test #t exact? 3)
(test #f inexact? 3)

(test #t = 22 22 22)
(test #t = 22 22)
(test #f = 34 34 35)
(test #f = 34 35)
(test #t > 3 -6246)
(test #f > 9 9 -2424)
(test #t >= 3 -4 -6246)
(test #t >= 9 9)
(test #f >= 8 9)
(test #t < -1 2 3 4 5 6 7 8)
(test #f < -1 2 3 4 4 5 6 7)
(test #t <= -1 2 3 4 5 6 7 8)
(test #t <= -1 2 3 4 4 5 6 7)
(test #f < 1 3 2)
(test #f >= 1 3 2)

(test #t zero? 0)
(test #f zero? 1)
(test #f zero? -1)
(test #f zero? -100)
(test #t positive? 4)
(test #f positive? -4)
(test #f positive? 0)
(test #f negative? 4)
(test #t negative? -4)
(test #f negative? 0)
(test #t odd? 3)
(test #f odd? 2)
(test #f odd? -4)
(test #t odd? -1)
(test #f even? 3)
(test #t even? 2)
(test #t even? -4)
(test #f even? -1)

(test 38 max 34 5 7 38 6)
(test -24 min 3  5 5 330 4 -24)

(test 7 + 3 4)
(test '3 + 3)
(test 0 +)
(test 4 * 4)
(test 1 *)

(test -1 - 3 4)
(test -3 - 3)
(test 7 abs -7)
(test 7 abs 7)
(test 0 abs 0)

(test 5 quotient 35 7)
(test -5 quotient -35 7)
(test -5 quotient 35 -7)
(test 5 quotient -35 -7)
(test 1 modulo 13 4)
(test 1 remainder 13 4)
(test 3 modulo -13 4)
(test -1 remainder -13 4)
(test -3 modulo 13 -4)
(test 1 remainder 13 -4)
(test -1 modulo -13 -4)
(test -1 remainder -13 -4)
(define (divtest n1 n2)
	(= n1 (+ (* n2 (quotient n1 n2))
		 (remainder n1 n2))))
(test #t divtest 238 9)
(test #t divtest -238 9)
(test #t divtest 238 -9)
(test #t divtest -238 -9)

(test 4 gcd 0 4)
(test 4 gcd -4 0)
(test 4 gcd 32 -36)
(test 0 gcd)
(test 288 lcm 32 -36)
(test 1 lcm)

;;;;From: fred@sce.carleton.ca (Fred J Kaudel)
;;; Modified by jaffer.
(define (test-inexact)
  (define f3.9 (string->number "3.9"))
  (define f4.0 (string->number "4.0"))
  (define f-3.25 (string->number "-3.25"))
  (define f.25 (string->number ".25"))
  (define f4.5 (string->number "4.5"))
  (define f3.5 (string->number "3.5"))
  (define f0.0 (string->number "0.0"))
  (define f0.8 (string->number "0.8"))
  (define f1.0 (string->number "1.0"))
  (define wto write-test-obj)
  (define dto display-test-obj)
  (define lto load-test-obj)
  (newline)
  (display ";testing inexact numbers; ")
  (newline)
  (SECTION 6 5 5)
  (test #t inexact? f3.9)
  (test #t 'inexact? (inexact? (max f3.9 4)))
  (test f4.0 'max (max f3.9 4))
  (test f4.0 'exact->inexact (exact->inexact 4))
  (test (- f4.0) round (- f4.5))
  (test (- f4.0) round (- f3.5))
  (test (- f4.0) round (- f3.9))
  (test f0.0 round f0.0)
  (test f0.0 round f.25)
  (test f1.0 round f0.8)
  (test f4.0 round f3.5)
  (test f4.0 round f4.5)
  (set! write-test-obj (list f.25 f-3.25));.25 inexact errors less likely.
  (set! display-test-obj (list f.25 f-3.25));.3 often has such errors (~10^-13)
  (set! load-test-obj (list 'define 'foo (list 'quote write-test-obj)))
  (test #t call-with-output-file
      "tmp3"
      (lambda (test-file)
	(write-char #\; test-file)
	(display write-test-obj test-file)
	(newline test-file)
	(write load-test-obj test-file)
	(output-port? test-file)))
  (check-test-file "tmp3")
  (set! write-test-obj wto)
  (set! display-test-obj dto)
  (set! load-test-obj lto)
  (let ((x (string->number "4195835.0"))
	(y (string->number "3145727.0")))
    (test #t 'pentium-fdiv-bug (> f1.0 (- x (* (/ x y) y)))))
  (report-errs))

(define (test-bignum)
  (define tb
    (lambda (n1 n2)
      (= n1 (+ (* n2 (quotient n1 n2))
	       (remainder n1 n2)))))
  (newline)
  (display ";testing bignums; ")
  (newline)
  (SECTION 6 5 5)
  (test 0 modulo -2177452800 86400)
  (test 0 modulo 2177452800 -86400)
  (test 0 modulo 2177452800 86400)
  (test 0 modulo -2177452800 -86400)
  (test #t 'remainder (tb 281474976710655 65535))
  (test #t 'remainder (tb 281474976710654 65535))
  (SECTION 6 5 6)
  (test 281474976710655 string->number "281474976710655")
  (test "281474976710655" number->string 281474976710655)
  (report-errs))


(report-errs)
(newline)
"last item in file"
