;; FILE: "string2number.scm"
;; IMPLEMENTS: String->Number for ProtoScheme
;; AUTHOR: Ken Dickey
;; DATE: 20 December 2001

;; COPYRIGHT (c) 2001 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;; (load "d:/SmallScript/ProtoScheme/Work/number2string.scm")

;; NOTES:
;;  Forms/components:
;;	complex:  [realORrat]+/-[realORrat]I
;;	real:     int.intEint
;;	rational: int/int
;;	integer:  int

;;  Each number component has: sign digits radix exact?
;;  Each number has: force->exactness? (->ie is #\e #\i or #f)
;;  So each number is specified as either exact or inexact and
;;  further may be coerced.  E.g. #e12## is specified as an
;;  inexact integer 1200 which is coerced to exact.

(define (exact-marker exact? ->ie)
  (case ->ie ;; ie => coerce
   ((#f)  (if exact? 'e 'i))
   ((#\i) (if exact? 'e->i 'i))
   ((#\e) (if exact? 'e 'i->e))
) )

;; The basic strategy is to [1] pick off the optional prefix
;; [2] parse an integer, moving to more complex forms as
;; required.

;; ToDo: Letrecify (after testing)


;(define (string->number str . radix) ;; return number or #f
;; TEST
(define (string2number str . radix) ;; return number or #f
  (prefix str 0 #\+ (if (null? radix) 10 (cadr radix)) #f))


(define num-2-chars  (string->list "01"))
(define num-8-chars  (string->list "01234567"))
(define num-10-chars (string->list "0123456789"))
(define num-16-chars (string->list "0123456789abcdefABCDEF"))

(define exponent-marker-chars (string->list "esfdlESFDL"))
(define radix-chars     (string->list "bodxBODX"))
(define exactness-chars (string->list "ieIE"))
(define sign-chars      (string->list "+-"))


(define (prefix str idx sign radix ->ie)
  (let ( (char (string-ref str idx)) )
    (cond
     ((memv char num-16-chars) ;; no more prefix
      (case radix
	((10) 
	 (integer str idx (string-length str) num-10-chars sign 10 '() #t ->ie)
	)
	((2)
	 (integer str idx (string-length str) num-2-chars  sign  2 '() #t ->ie)
	)
	((8)
	 (integer str idx (string-length str) num-8-chars  sign  8 '() #t ->ie)
	)
	((16)
	 (integer str idx (string-length str) num-16-chars sign 16 '() #t ->ie)
	)
	(else #f) ;; illegal radix
     ))
     ((eq? char #\. )
      (decimal str (+ 1 idx) (string-length str) #\+ "0" '() #t ->ie)
      ;;(decimal str idx len sign int-part digits exact? ->ie)
     )
     ((memv char sign-chars) 
      (prefix str (+ 1 idx) char radix ->ie)
     )
     ((eq? char #\#) ;; radix or exactness in any order
      (let ( (char2 (string-ref str (+ 1 idx))) )
	(cond
	 ((memv char2 radix-chars)
	  (prefix str (+ 2 idx) sign (char->radix char2) ->ie)
	 )
	 ((memv char2 exactness-chars)
	  (prefix str (+ 2 idx) sign radix char2)
	 )
	 (else #f) ;; error
     )) )
     ((eq? (char-downcase char) #\i)
      (if (= (+ 1 idx) (string-length str))
	  (make-complex (make-zero) (make-one sign))
	  #f ;; error
     ))
     (else #f) ;; illegal number syntax
) ) )


(define char->radix
  (let ( (table '((#\b . 2) (#\o . 8) (#\d . 10) (#\x . 16)
		  (#\B . 2) (#\O . 8) (#\D . 10) (#\X . 16)))
       )
    (lambda (char)
      (cond
       ((assq char table) => cdr)
       (else #f)
) ) ) )


(define (integer str idx len legal-chars sign radix digits exact? ->ie)
  (if (>= idx len)
      (make-integer sign (list->string (reverse digits)) 
		    radix (exact-marker exact? ->ie))
      (let ( (char (string-ref str idx)) )
	(cond
	 ((memv (string-ref str idx) legal-chars)
	  (integer str (+ idx 1) len 
		   legal-chars sign radix 
		   (cons char digits) exact? ->ie)
	 )
	 ((eq? char #\#)
	  (integer str (+ idx 1) len 
		   legal-chars sign radix 
		   (cons #\0 digits) #f ->ie)
	 )
	 ((eq? char #\. )
	  (if (and radix (= radix 10))
	      (decimal str (+ 1 idx) len sign
		       (list->string (reverse digits)) '() exact? ->ie)
	      #f  ;; must be radix 10 for decimal
	 ))
	 ((eq? char #\/ )
	  (rational str (+ 1 idx) len legal-chars sign radix
		    (list->string (reverse digits)) '() exact? ->ie)
	 )
	 ((memv char sign-chars)
	  (if (null? digits)
	      (complex (make-zero) str idx radix)
	      (complex (make-integer sign (list->string (reverse digits)) 
				     radix (exact-marker exact? ->ie))
		       str idx radix)
	 ))
	 ((eq? (char-downcase char) #\i)
	  (cond
	   ((not (= (+ 1 idx) (string-length str))) 
	    #f
	   )
	   ((null? digits)
	    (make-complex (make-zero) (make-one sign))
	   )
	   (else
	    (make-complex (make-zero)
			  (make-integer sign (list->string (reverse digits)) 
					radix (exact-marker exact? ->ie)))
	 )))
	 ((memv char exponent-marker-chars)
	  (if (and radix (= radix 10) (> (length digits) 0))
	      (exponent str (+ 1 idx) len sign 
			(list->string (reverse digits)) "0" '() #\+
			exact? ->ie)
	      #f  ;; must be radix 10 for exponent
	 ))
	 (else #f) ;; illegal
      ) )
) )


(define (rational str idx len legal-chars sign radix numerator digits exact? ->ie)
;; NUMERATOR /  seen
  (if (>= idx len)
      (make-rational sign 
		     numerator 
		     (list->string (reverse digits)) 
		     radix 
		     (exact-marker exact? ->ie))
      (let ( (char (string-ref str idx)) )
	(cond
	 ((memv (string-ref str idx) legal-chars)
	  (rational str (+ idx 1) len legal-chars sign radix numerator
		   (cons char digits) exact? ->ie)
	 )
	 ((memv char sign-chars)
	  (complex (make-rational sign 
				  numerator 
				  (list->string (reverse digits)) 
				  radix 
				  (exact-marker exact? ->ie))
		   str idx radix)
	 )
	 ((eq? char #\#)
	  (rational str (+ idx 1) len legal-chars sign radix numerator
		   (cons #\0 digits) #f ->ie)
	 )
	 ((eq? (char-downcase char) #\i)
	  (cond
	   ((not (= (+ 1 idx) (string-length str))) 
	    #f
	   )
	   ((null? digits)
	    #f ;; (17/i illegal)
	   )
	   (else
	    (make-complex (make-zero)
			  (make-rational sign 
					 numerator 
					 (list->string (reverse digits)) 
					 radix 
					 (exact-marker exact? ->ie)))
         )))
	 (else #f) ;; illegal
      ) )
) )

(define (decimal str idx len sign int-part digits exact? ->ie)
;; INT .  seen
  (if (>= idx len)
      (make-decimal sign
		    int-part
		    (list->string (reverse digits)) 
		    #\+ "0"
		    (exact-marker exact? ->ie))
      (let ( (char (string-ref str idx)) )
	(cond
	 ((memv (string-ref str idx) num-10-chars)
	  (decimal str (+ idx 1) len sign int-part
		   (cons char digits) exact? ->ie)
	 )
	 ((eq? char #\#)
	  (decimal str (+ idx 1) len sign int-part
		   (cons #\0 digits) #f ->ie)
	 )
	 ((memv char exponent-marker-chars)
	  (exponent str (+ 1 idx) len sign int-part
			(list->string (reverse digits)) '() #\+
			exact? ->ie)
	 )
	 ((eq? (char-downcase char) #\i)
	  (if (= (+ 1 idx) (string-length str))
	      (make-complex (make-zero)
			    (make-decimal sign
					  int-part
					  (list->string (reverse digits)) 
					  #\+ "0"
					  (exact-marker exact? ->ie)))
	      #f ;; no chars after #\i
         ))
	 (else #f) ;; illegal
      ) )
) )

(define (exponent str idx len sign int-part frac-part digits exp-sign exact? ->ie)
;; INT . INT e  seen
  (if (>= idx len)
      (make-decimal sign
		    int-part
		    frac-part
		    exp-sign
		    (list->string (reverse digits))
		    (exact-marker exact? ->ie))
      (let ( (char (string-ref str idx)) )
	(cond
	 ((memv char num-10-chars)
	  (exponent str (+ idx 1) len sign int-part frac-part
		   (cons char digits) exp-sign exact? ->ie)
	 )
	 ((memv char sign-chars)
	  (if (null? digits)
	      (exponent str (+ idx 1) len sign int-part frac-part
			digits char exact? ->ie)
	      (complex (make-decimal sign
				     int-part
				     frac-part
				     exp-sign
				     (list->string (reverse digits))
				     (exact-marker exact? ->ie))
		       str idx radix)
	 ))
	 ((eq? (char-downcase char) #\i)
	  (if (or (null? digits) (not (= (+ 1 idx) (string-length str))))
	      #f ;; (12.3ei illegal)
	      (make-complex (make-zero)
			    (make-decimal sign
					  int-part
					  frac-part
					  exp-sign
					  (list->string (reverse digits))
					  (exact-marker exact? ->ie)))
         ))
	 (else #f) ;; illegal
      ) )
) )

(define (complex real-part str idx radix)
  (let ( (imaginitive-part (prefix str idx #\+ radix #f)) )
    ;; (unless (complex? imaginitive-part) #f)
    (if (and imaginitive-part (eq? (car imaginitive-part) 'complex))
	(make-complex real-part (caddr imaginitive-part))
	#f) ;; illegal
 ) )

;;==========================================================;;

(define (make-integer sign str radix exactness)
  ;; debug
  (if (zero? (string-length str))
      #f
      `(integer ,sign ,str ,radix ,exactness)))

(define (make-rational sign numerator denominator radix exactness)
  ;; debug
  (if (or (zero? (string-length numerator))
	  (zero? (string-length denominator)))
      #f
      `(rational  ,sign ,numerator ,denominator ,radix ,exactness)))


(define (make-decimal sign int-part frac-part exp-sign exponent-part exactness)
  ;; debug
  (if (and (zero? (string-length int-part))
	   (zero? (string-length frac-part)))
      #f
      `(decimal ,sign ,int-part ,frac-part ,exp-sign ,exponent-part ,exactness)))

(define (make-complex real-part imaginitive-part)
  `(complex ,real-part ,imaginitive-part))

(define (make-zero)     (make-integer #\+  "0" 10 #\e))
(define (make-one sign) (make-integer sign "1" 10 #\e))

(define (string-upcase str)
  (list->string
   (map char-upcase
	(string->list str))))

(define (->ss-int int-str radix)
  (string-append 
   (case radix ((2) "2r")((8)"8r")((16)"16r")(else "")) 
   (string-upcase int-str)))

;; temp
(define (->decimal-string int-str radix)
  (number->string
   (string->number int-str radix)))

(define (->ss-float sign-char int-str frac-str expt-sign-char expt-str)
  ;; NB: assumes radix 10
  (let* ( (int  (if (zero? (string-length int-str))  "0" int-str))
	  (frac (if (zero? (string-length frac-str)) "0" frac-str))
	  (sign (if (eq? sign-char #\-) "-" ""))
	  (expt-sign (if (eq? expt-sign-char #\-) "-" ""))
	  (expt (if (zero? (string-length expt-str))
		    ""
		    (string-append "e" expt-sign expt-str)))
        )
    (string-append sign int "." frac expt)
) )


;;==========================================================;;
(define pass-cases
  '("#o36" "#xF8" "#x8f" "#b1101" "#d26" "23.512"
    "2.3e5" "#i3" "34##" "#e45" "2/3"
    "-12" "-12.3" "+5" "#e#o37" "#o#e37" "#i#d7.2" "3.7e5"
    "3e5" "15##" "#x15##" "#x#e15##" "#e#x15##" "#e15##"
    "23##.#e2" "23##.e2" "23##." "23##.##e-2" "34.3#e2"
    ".5##" "3." "+3." "#e1.3" "1#/231#" "#i#o37" "#o#i3" 
    "#i1500" "#i#x3f27" "#x#i3f27"
)  )

(define complex-cases
  '("+34i" "-12i" "+i" "-i" "3+4i" "2-i"
    "3/4+1/2i" "2/3-12i" "3+2/3i" "+12/15-3i" "#xE/F+a/di"
)  )

(define fail-cases
  '("+1-3" "ii" "##15" "i+i" "3i+3i" "34i+3"
    "1/2/3" "d26"
)  )

(define (test-em)
  (for-each
   (lambda (s) (newline) (write s) (display #\tab) (write (n2s s)))
   (append pass-cases complex-cases))

  (for-each
   (lambda (s) (if (n2s s) 
		   (begin (newline)
			  (write s) 
			  (display #\tab) 
			  (write (n2s s))
			  (display #\tab) 
			  (display " error"))))
   fail-cases)
  (newline)
)

		;;   ---   E O F   ---   ;;
