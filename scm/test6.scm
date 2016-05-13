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
(SECTION 6 5 6)
(test "0" number->string 0)
(test "100" number->string 100)
(test "100" number->string 256 16)
(test 100 string->number "100")
(test 256 string->number "100" 16)
(test #f string->number "")
(test #f string->number ".")
(test #f string->number "d")
(test #f string->number "D")
(test #f string->number "i")
(test #f string->number "I")
(test #f string->number "3i")
(test #f string->number "3I")
(test #f string->number "33i")
(test #f string->number "33I")
(test #f string->number "3.3i")
(test #f string->number "3.3I")
(test #f string->number "-")
(test #f string->number "+")

(SECTION 6 6)
(test #t eqv? '#\  #\Space)
(test #t eqv? #\space '#\Space)
(test #t char? #\a)
(test #t char? #\()
(test #t char? #\ )
(test #t char? '#\newline)

(test #f char=? #\A #\B)
(test #f char=? #\a #\b)
(test #f char=? #\9 #\0)
(test #t char=? #\A #\A)

(test #t char<? #\A #\B)
(test #t char<? #\a #\b)
(test #f char<? #\9 #\0)
(test #f char<? #\A #\A)

(test #f char>? #\A #\B)
(test #f char>? #\a #\b)
(test #t char>? #\9 #\0)
(test #f char>? #\A #\A)

(test #t char<=? #\A #\B)
(test #t char<=? #\a #\b)
(test #f char<=? #\9 #\0)
(test #t char<=? #\A #\A)

(test #f char>=? #\A #\B)
(test #f char>=? #\a #\b)
(test #t char>=? #\9 #\0)
(test #t char>=? #\A #\A)

(test #f char-ci=? #\A #\B)
(test #f char-ci=? #\a #\B)
(test #f char-ci=? #\A #\b)
(test #f char-ci=? #\a #\b)
(test #f char-ci=? #\9 #\0)
(test #t char-ci=? #\A #\A)
(test #t char-ci=? #\A #\a)

(test #t char-ci<? #\A #\B)
(test #t char-ci<? #\a #\B)
(test #t char-ci<? #\A #\b)
(test #t char-ci<? #\a #\b)
(test #f char-ci<? #\9 #\0)
(test #f char-ci<? #\A #\A)
(test #f char-ci<? #\A #\a)

(test #f char-ci>? #\A #\B)
(test #f char-ci>? #\a #\B)
(test #f char-ci>? #\A #\b)
(test #f char-ci>? #\a #\b)
(test #t char-ci>? #\9 #\0)
(test #f char-ci>? #\A #\A)
(test #f char-ci>? #\A #\a)

(test #t char-ci<=? #\A #\B)
(test #t char-ci<=? #\a #\B)
(test #t char-ci<=? #\A #\b)
(test #t char-ci<=? #\a #\b)
(test #f char-ci<=? #\9 #\0)
(test #t char-ci<=? #\A #\A)
(test #t char-ci<=? #\A #\a)

(test #f char-ci>=? #\A #\B)
(test #f char-ci>=? #\a #\B)
(test #f char-ci>=? #\A #\b)
(test #f char-ci>=? #\a #\b)
(test #t char-ci>=? #\9 #\0)
(test #t char-ci>=? #\A #\A)
(test #t char-ci>=? #\A #\a)

(test #t char-alphabetic? #\a)
(test #t char-alphabetic? #\A)
(test #t char-alphabetic? #\z)
(test #t char-alphabetic? #\Z)
(test #f char-alphabetic? #\0)
(test #f char-alphabetic? #\9)
(test #f char-alphabetic? #\space)
(test #f char-alphabetic? #\;)

(test #f char-numeric? #\a)
(test #f char-numeric? #\A)
(test #f char-numeric? #\z)
(test #f char-numeric? #\Z)
(test #t char-numeric? #\0)
(test #t char-numeric? #\9)
(test #f char-numeric? #\space)
(test #f char-numeric? #\;)

(test #f char-whitespace? #\a)
(test #f char-whitespace? #\A)
(test #f char-whitespace? #\z)
(test #f char-whitespace? #\Z)
(test #f char-whitespace? #\0)
(test #f char-whitespace? #\9)
(test #t char-whitespace? #\space)
(test #f char-whitespace? #\;)

(test #f char-upper-case? #\0)
(test #f char-upper-case? #\9)
(test #f char-upper-case? #\space)
(test #f char-upper-case? #\;)

(test #f char-lower-case? #\0)
(test #f char-lower-case? #\9)
(test #f char-lower-case? #\space)
(test #f char-lower-case? #\;)

(test #\. integer->char (char->integer #\.))
(test #\A integer->char (char->integer #\A))
(test #\a integer->char (char->integer #\a))
(test #\A char-upcase #\A)
(test #\A char-upcase #\a)
(test #\a char-downcase #\A)
(test #\a char-downcase #\a)

(report-errs)
(newline)
"last item in file"
