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
(SECTION 6 4)
(test #t symbol? 'foo)
(test #t symbol? (car '(a b)))
(test #f symbol? "bar")
(test #t symbol? 'nil)
(test #f symbol? '())
(test #f symbol? #f)
;;; But first, what case are symbols in?  Determine the standard case:
(define char-standard-case char-upcase)
(if (string=? (symbol->string 'A) "a")
    (set! char-standard-case char-downcase))
(test #t 'standard-case
      (string=? (symbol->string 'a) (symbol->string 'A)))
(test #t 'standard-case
      (or (string=? (symbol->string 'a) "A")
	  (string=? (symbol->string 'A) "a")))
(define (str-copy s)
  (let ((v (make-string (string-length s))))
    (do ((i (- (string-length v) 1) (- i 1)))
	((< i 0) v)
      (string-set! v i (string-ref s i)))))
(define (string-standard-case s)
  (set! s (str-copy s))
  (do ((i 0 (+ 1 i))
       (sl (string-length s)))
      ((>= i sl) s)
      (string-set! s i (char-standard-case (string-ref s i)))))
(test (string-standard-case "flying-fish") symbol->string 'flying-fish)
(test (string-standard-case "martin") symbol->string 'Martin)
(test "Malvina" symbol->string (string->symbol "Malvina"))
(test #t 'standard-case (eq? 'a 'A))

(define x (string #\a #\b))
(define y (string->symbol x))
(string-set! x 0 #\c)
(test "cb" 'string-set! x)
(test "ab" symbol->string y)
(test y string->symbol "ab")

(test #t eq? 'mISSISSIppi 'mississippi)
(test #f 'string->symbol (eq? 'bitBlt (string->symbol "bitBlt")))
(test 'JollyWog string->symbol (symbol->string 'JollyWog))

(SECTION 6 5 5)
(test #t number? 3)
(test #t complex? 3)
(test #t real? 3)
(test #t rational? 3)

(report-errs)
(newline)
"last item in file"
