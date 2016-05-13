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

(SECTION 6 7)
(test #t string? "The word \"recursion\\\" has many meanings.")
(test #t string? "")
(define f (make-string 3 #\*))
(test "?**" 'string-set! (begin (string-set! f 0 #\?) f))
(test "abc" string #\a #\b #\c)
(test "" string)
(test 3 string-length "abc")
(test #\a string-ref "abc" 0)
(test #\c string-ref "abc" 2)
(test 0 string-length "")
(test "" substring "ab" 0 0)
(test "" substring "ab" 1 1)
(test "" substring "ab" 2 2)
(test "a" substring "ab" 0 1)
(test "b" substring "ab" 1 2)
(test "ab" substring "ab" 0 2)
(test "foobar" string-append "foo" "bar")
(test "foo" string-append "foo")
(test "foo" string-append "foo" "")
(test "foo" string-append "" "foo")
(test "" string-append)
(test "" make-string 0)
(test #t string=? "" "")
(test #f string<? "" "")
(test #f string>? "" "")
(test #t string<=? "" "")
(test #t string>=? "" "")
(test #t string-ci=? "" "")
(test #f string-ci<? "" "")
(test #f string-ci>? "" "")
(test #t string-ci<=? "" "")
(test #t string-ci>=? "" "")

(test #f string=? "A" "B")
(test #f string=? "a" "b")
(test #f string=? "9" "0")
(test #t string=? "A" "A")

(test #t string<? "A" "B")
(test #t string<? "a" "b")
(test #f string<? "9" "0")
(test #f string<? "A" "A")

(test #f string>? "A" "B")
(test #f string>? "a" "b")
(test #t string>? "9" "0")
(test #f string>? "A" "A")

(test #t string<=? "A" "B")
(test #t string<=? "a" "b")
(test #f string<=? "9" "0")
(test #t string<=? "A" "A")

(test #f string>=? "A" "B")
(test #f string>=? "a" "b")
(test #t string>=? "9" "0")
(test #t string>=? "A" "A")

(test #f string-ci=? "A" "B")
(test #f string-ci=? "a" "B")
(test #f string-ci=? "A" "b")
(test #f string-ci=? "a" "b")
(test #f string-ci=? "9" "0")
(test #t string-ci=? "A" "A")
(test #t string-ci=? "A" "a")

(test #t string-ci<? "A" "B")
(test #t string-ci<? "a" "B")
(test #t string-ci<? "A" "b")
(test #t string-ci<? "a" "b")
(test #f string-ci<? "9" "0")
(test #f string-ci<? "A" "A")
(test #f string-ci<? "A" "a")

(test #f string-ci>? "A" "B")
(test #f string-ci>? "a" "B")
(test #f string-ci>? "A" "b")
(test #f string-ci>? "a" "b")
(test #t string-ci>? "9" "0")
(test #f string-ci>? "A" "A")
(test #f string-ci>? "A" "a")

(test #t string-ci<=? "A" "B")
(test #t string-ci<=? "a" "B")
(test #t string-ci<=? "A" "b")
(test #t string-ci<=? "a" "b")
(test #f string-ci<=? "9" "0")
(test #t string-ci<=? "A" "A")
(test #t string-ci<=? "A" "a")

(test #f string-ci>=? "A" "B")
(test #f string-ci>=? "a" "B")
(test #f string-ci>=? "A" "b")
(test #f string-ci>=? "a" "b")
(test #t string-ci>=? "9" "0")
(test #t string-ci>=? "A" "A")
(test #t string-ci>=? "A" "a")

(report-errs)
(newline)
"last item in file"
