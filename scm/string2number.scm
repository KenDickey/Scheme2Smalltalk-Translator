;; FILE: "string2number.scm"
;; IMPLEMENTS: String->number for ProtoScheme
;; AUTHOR: Ken Dickey
;; DATE: 20 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;; (load "/home/kend/Squeak-3.0/ProtoScheme/SCM/string2number.scm")

;; NOTES:
;;  Forms/components:
;;	complex:  [realORrat]+/-[realORrat]I
;;	real:     int.intEint
;;	rational: int/int
;;	integer:  int

;;  Each number component has: sign digits radix exact?
;;  Each number has: force->exactness? (make-inexact? is #\e #\i or #f)
;;  So each number is specified as either exact or inexact and
;;  further may be coerced.  E.g. #e12## is specified as an
;;  inexact integer 1200 which is coerced to exact.

; (define (exact-marker exact? make-inexact?)
;   (case make-inexact? ;; ie => coerce
;    ((#f)  (if exact? 'e 'i))
;    ((#\i) (if exact? 'e->i 'i))
;    ((#\e) (if exact? 'e 'i->e))
; ) )

;; The basic strategy is to [1] pick off the optional prefix
;; [2] parse an integer, moving to more complex forms as
;; required.


;(define (string->number str . radix) ;; return number or #f
(define string->number
  (letrec 
      ( (num-2-chars  (string->list "01"))
	(num-8-chars  (string->list "01234567"))
	(num-10-chars (string->list "0123456789"))
	(num-16-chars (string->list "0123456789abcdefABCDEF"))
	(exponent-marker-chars (string->list "esfdlESFDL"))
	(radix-chars     (string->list "bodxBODX"))
	(exactness-chars (string->list "ieIE"))
	(sign-chars      (string->list "+-"))
	(radix-table '((#\b . 2) (#\o . 8) (#\d . 10) (#\x . 16)
		       (#\B . 2) (#\O . 8) (#\D . 10) (#\X . 16)))
	(char->radix
	 (lambda (char)
	   (cond
	    ((assq char radix-table) => cdr)
	    (else #f)
	)))
	(string-upcase 
	 (lambda (str)
	   (list->string
	    (map char-upcase
		 (string->list str))))
	)
	(to-ss-int 
	 (lambda (int-str radix)
	   (string-append 
	    (case radix ((2) "2r")((8)"8r")((16)"16r")(else "")) 
	    (string-upcase int-str)))
	)
	(to-ss-float
	 (lambda (sign-char int-str frac-str expt-sign-char expt-str)
	   ;; NB: assumes radix 10
	   (let* ( (int  (if (zero? (string-length int-str))  "0" int-str))
		   (frac (if (zero? (string-length frac-str)) "0" frac-str))
		   (sign (if (eq? sign-char #\-) "-" ""))
		   (expt-sign (if (eq? expt-sign-char #\-) "-" ""))
		   (expt (if (zero? (string-length expt-str))
			     ""
			     (string-append "e" expt-sign expt-str)))
		   )
	     (if (and (zero? (string-length int-str))
		      (zero? (string-length frac-str)))
		 #f ;; "." is not legal number syntax
		 (string-append sign int "." frac expt))
	)))
	(coerce-if-needed
	 (lambda (num exact? make-inexact?)
	   (case make-inexact?
	     ((#\e) (if exact? num (inexact->exact num)))
	     ((#\i) (if exact? (exact->inexact num) num))
	     (else num)
	)))
	(make-integer 
	 (lambda (sign str radix exact? make-inexact?)
	   (let ( (num (: (to-ss-int str radix) "asNumber")) ) ;;@@"asInteger")) )
	     (coerce-if-needed (if (eq? sign #\-) (- 0 num) num) exact? make-inexact?)
	)))
	(make-decimal 
	 (lambda (sign int-part frac-part exp-sign exponent-part exact? make-inexact?)
	   (let ( (num 
		   (: (to-ss-float sign int-part frac-part exp-sign exponent-part)
		      "asNumber"))
		)
	     (coerce-if-needed num exact? make-inexact?)
	)))
	(make-rational 
	 (lambda (sign numerator denominator radix exact? make-inexact?)
	   (let* ( (num-part   (: (to-ss-int numerator   radix) "asNumber" ))
		   (denom-part (: (to-ss-int denominator radix) "asNumber"))
		   (num (if (eq? sign #\-)
			    (- (/ num-part denom-part))
			    (/ num-part denom-part)))
		 )
	   (coerce-if-needed num exact? make-inexact?)
	)))
	(make-complex 
	 (lambda (real-part imaginative-part exact? make-inexact?)
	   (coerce-if-needed
	    (: ($ "Complex") real: real-part imaginary: imaginative-part)
	    exact? 
	    make-inexact?)
	))
	;; PREFIX
	(prefix 
	 (lambda (str idx sign radix make-inexact?)
	   (if (>= idx (string-length str))
	       #f
	       (let ( (char (string-ref str idx)) )
		 (cond
		  ((memv char num-16-chars) ;; no more prefix
		   (case radix
		     ((10) 
		      (integer str idx (string-length str) 
			       num-10-chars sign 10 '() #t make-inexact?)
		      )
		     ((2)
		      (integer str idx (string-length str) 
			       num-2-chars  sign  2 '() #t make-inexact?)
		      )
		     ((8)
		      (integer str idx (string-length str) 
			       num-8-chars  sign  8 '() #t make-inexact?)
		      )
		     ((16)
		      (integer str idx (string-length str) 
			       num-16-chars sign 16 '() #t make-inexact?)
		      )
		     (else #f) ;; illegal radix
		     ) )
		  ((eq? char #\. )
		   (decimal str (+ 1 idx) (string-length str) #\+ "0" '() #t make-inexact?)
		   ;;(decimal str idx len sign int-part digits exact? make-inexact?)
		   )
		  ((memv char sign-chars) 
		   (prefix str (+ 1 idx) char radix make-inexact?)
		   )
		  ((eq? char #\#) ;; radix or exactness in any order
		   (let ( (char2 (string-ref str (+ 1 idx))) )
		     (cond
		      ((memv char2 radix-chars)
		       (prefix str (+ 2 idx) sign (char->radix char2) make-inexact?)
		       )
		      ((memv char2 exactness-chars)
		       (prefix str (+ 2 idx) sign radix char2)
		       )
		      (else #f) ;; error
		      )) )
		  ((eq? (char-downcase char) #\i)
		   (if (= (+ 1 idx) (string-length str))
		       (if (= 1 (string-length str))
			   #f ;; "i" is illegal
			   (if (not sign)
			       #f
			       (make-complex 0 0 exact? make-inexact?)))
		       #f) ;; illegal
		  )
		  (else #f) ;; illegal number syntax
	)))))
	;; INTEGER
	(integer 
	 (lambda (str idx len legal-chars sign radix digits exact? make-inexact?)
	   (if (>= idx len)
	       (make-integer sign (list->string (reverse digits)) 
			     radix exact? make-inexact?)
	       (let ( (char (string-ref str idx)) )
		 (cond
		  ((memv (string-ref str idx) legal-chars)
		   (integer str (+ idx 1) len 
			    legal-chars sign radix 
			    (cons char digits) exact? make-inexact?)
		  )
		  ((eq? char #\#)
		   (integer str (+ idx 1) len 
			    legal-chars sign radix 
			    (cons #\0 digits) #f make-inexact?)
		  )
		  ((eq? char #\. )
		   (if (and radix (= radix 10))
		       (decimal str (+ 1 idx) len sign
				(list->string (reverse digits)) '() exact? make-inexact?)
		       #f  ;; must be radix 10 for decimal
		  ))
		  ((eq? char #\/ )
		   (rational str (+ 1 idx) len legal-chars sign radix
			     (list->string (reverse digits)) '() exact? make-inexact?)
		  )
		  ((memv char sign-chars)
		   (if (null? digits)
		       (complex 0 str idx radix)
		       (complex (make-integer sign (list->string (reverse digits)) 
					      radix exact? make-inexact?)
				str idx radix)
		  ))
		  ((eq? (char-downcase char) #\i)
		   (cond
		    ((not (= (+ 1 idx) (string-length str))) 
		     #f
		    )
		    ((null? digits)
		     (make-complex 0 0 exact? make-inexact?)
		    )
		    ((not sign)
		     #f)
		    (else
		     (make-complex 0
				   (make-integer sign (list->string (reverse digits)) 
						 radix exact? make-inexact?)
				   exact? make-inexact?)
		  )))
		  ((memv char exponent-marker-chars)
		   (if (and radix (= radix 10) (> (length digits) 0))
		       (exponent str (+ 1 idx) len sign 
				 (list->string (reverse digits)) "0" '() #\+
				 exact? make-inexact?)
		       #f  ;; must be radix 10 for exponent
		  ))
		  (else #f) ;; illegal
	)))))
	;; RATIONAL
	(rational
	 (lambda (str idx len legal-chars sign radix numerator digits exact? make-inexact?)
	   ;; NUMERATOR /  seen
	   (if (>= idx len)
	       (make-rational sign 
			      numerator 
			      (list->string (reverse digits)) 
			      radix 
			      exact? make-inexact?)
	       (let ( (char (string-ref str idx)) )
		 (cond
		  ((memv (string-ref str idx) legal-chars)
		   (rational str (+ idx 1) len legal-chars sign radix numerator
			     (cons char digits) exact? make-inexact?)
		  )
		  ((memv char sign-chars)
		   (complex (make-rational sign 
					   numerator 
					   (list->string (reverse digits)) 
					   radix 
					   exact? make-inexact?)
			    str idx radix)
		  )
		  ((eq? char #\#)
		   (rational str (+ idx 1) len legal-chars sign radix numerator
			     (cons #\0 digits) #f make-inexact?)
		  )
		  ((eq? (char-downcase char) #\i)
		   (cond
		    ((not (= (+ 1 idx) (string-length str))) 
		     #f
		    )
		    ((null? digits)
		     #f ;; (17/i illegal)
		    )
		    ((not sign)
		     #f)
		    (else
		     (make-complex 0
				   (make-rational sign 
						  numerator 
						  (list->string (reverse digits)) 
						  radix 
						  exact? make-inexact?)
				   exact? make-inexact?)
		  )))
		  (else #f) ;; illegal
	)))))
	;; DECIMAL
	(decimal
	 (lambda (str idx len sign int-part digits exact? make-inexact?)
	   ;; INT .  seen
	   (if (>= idx len)
	       (if (= 1 (string-length str))
		   #f ;; "." illegal
		   (make-decimal sign
				 int-part
				 (list->string (reverse digits)) 
				 #\+ "0"
				 exact? make-inexact?))
	       (let ( (char (string-ref str idx)) )
		 (cond
		  ((memv (string-ref str idx) num-10-chars)
		   (decimal str (+ idx 1) len sign int-part
			    (cons char digits) exact? make-inexact?)
		  )
		  ((eq? char #\#)
		   (decimal str (+ idx 1) len sign int-part
			    (cons #\0 digits) #f make-inexact?)
		  )
		  ((memv char exponent-marker-chars)
		   (exponent str (+ 1 idx) len sign int-part
			     (list->string (reverse digits)) '() #\+
			     exact? make-inexact?)
		  )
		  ((eq? (char-downcase char) #\i)
		   (if (and sign (= (+ 1 idx) (string-length str)))
		       (make-complex 0
				     (make-decimal sign
						   int-part
						   (list->string (reverse digits)) 
						   #\+ "0"
						   exact? make-inexact?)
				     exact? make-inexact?)
		       #f ;; no chars after #\i
		  ))
		  (else #f) ;; illegal
	)))))
	;; EXPONENT
	(exponent
	 (lambda (str idx len sign int-part frac-part digits exp-sign exact? make-inexact?)
	   ;; INT . INT e  seen
	   (if (>= idx len)
	       (make-decimal sign
			     int-part
			     frac-part
			     exp-sign
			     (list->string (reverse digits))
			     exact? make-inexact?)
	       (let ( (char (string-ref str idx)) )
		 (cond
		  ((memv char num-10-chars)
		   (exponent str (+ idx 1) len sign int-part frac-part
			     (cons char digits) exp-sign exact? make-inexact?)
		  )
		  ((memv char sign-chars)
		   (if (null? digits)
		       (exponent str (+ idx 1) len sign int-part frac-part
				 digits char exact? make-inexact?)
		       (complex (make-decimal sign
					      int-part
					      frac-part
					      exp-sign
					      (list->string (reverse digits))
					      exact? make-inexact?)
				str idx radix)
		  ))
		  ((eq? (char-downcase char) #\i)
		   (if (or (null? digits) 
			   (not sign)
			   (not (= (+ 1 idx) (string-length str))))
		       #f ;; (12.3ei illegal)
		       (make-complex 0
				     (make-decimal sign
						   int-part
						   frac-part
						   exp-sign
						   (list->string (reverse digits))
						   exact? make-inexact?)
				     exact? make-inexact?)
		  ))
		  (else #f) ;; illegal
        )))))
	;; COMPLEX
	(complex
	 (lambda (real-part str idx radix)
	   (let ( (imaginitive-part (prefix str idx #\+ radix #f)) )
	     ;; (unless (complex? imaginitive-part) #f)
	     (if imaginitive-part
		 (: real-part "+" imaginitive-part)
		 #f) ;; illegal
	)) )

      )
    (lambda (str . radix)
      (if (not (string? str))
	  (error "Expected a string" str))
      (if (< (string-length str) 1)
	  #f
	  (prefix str 0 #f (if (null? radix) 10 (car radix)) #f))
    )
) )



;;==========================================================;;
;; DEBUG

; (make-integer sign str radix exactness)
;   ;; debug
;   (if (zero? (string-length str))
;       #f
;       `(integer ,sign ,str ,radix ,exactness)))

; (define (make-rational sign numerator denominator radix exactness)
;   ;; debug
;   (if (or (zero? (string-length numerator))
; 	  (zero? (string-length denominator)))
;       #f
;       `(rational  ,sign ,numerator ,denominator ,radix ,exactness)))


; (define (make-decimal sign int-part frac-part exp-sign exponent-part exactness)
;   ;; debug
;   (if (and (zero? (string-length int-part))
; 	   (zero? (string-length frac-part)))
;       #f
;       `(decimal ,sign ,int-part ,frac-part ,exp-sign ,exponent-part ,exactness)))

; (define (make-complex real-part imaginitive-part)
;   `(complex ,real-part ,imaginitive-part))

; (define (make-zero)     (make-integer #\+  "0" 10 #\e))
; (define (make-one sign) (make-integer sign "1" 10 #\e))


;;==========================================================;;
;; TESTING

; (define pass-cases
;   '("#o36" "#xF8" "#x8f" "#b1101" "#d26" "23.512"
;     "2.3e5" "#i3" "34##" "#e45" "2/3"
;     "-12" "-12.3" "+5" "#e#o37" "#o#e37" "#i#d7.2" "3.7e5"
;     "3e5" "15##" "#x15##" "#x#e15##" "#e#x15##" "#e15##"
;     "23##.#e2" "23##.e2" "23##." "23##.##e-2" "34.3#e2"
;     ".5##" "3." "+3." "#e1.3" "1#/231#" "#i#o37" "#o#i3" 
;     "#i1500" "#i#x3f27" "#x#i3f27"
; )  )

; (define complex-cases
;   '("+34i" "-12i" "+i" "-i" "3+4i" "2-i"
;     "3/4+1/2i" "2/3-12i" "3+2/3i" "+12/15-3i" "#xE/F+a/di"
; )  )

; (define fail-cases
;   '("+1-3" "ii" "##15" "i+i" "3i+3i" "34i+3"
;     "1/2/3" "d26"
; )  )

; (define (test-em)
;   (for-each
;    (lambda (s) (newline) (write s) (display #\tab) (write (n2s s)))
;    (append pass-cases complex-cases))

;   (for-each
;    (lambda (s) (if (n2s s) 
; 		   (begin (newline)
; 			  (write s) 
; 			  (display #\tab) 
; 			  (write (n2s s))
; 			  (display #\tab) 
; 			  (display " error"))))
;    fail-cases)
;   (newline)
; )

		;;   ---   E O F   ---   ;;
