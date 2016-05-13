;; FILE: "numbers.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Number Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMBERS
;;;;;;;;;;


(define (nil? obj) (: obj "==" ($ nil)))

(define (number? obj)   
  (: obj "isKindOf:" ($ "Number")))

(define (complex?  obj) 
  (or (: obj "isKindOf:" ($ "Complex"))
      (real? obj)
      (rational? obj)
      (integer? obj)
) )

(define (real?     obj) 
  (or (: obj "isKindOf:" ($ "Float"))
      (rational? obj)
      (integer? obj)
) )

(define (rational? obj) 
  (or (: obj "isKindOf:" ($ "Fraction"))
      (integer? obj)
) )

(define (integer?  obj) 
  (: obj "isKindOf:" ($ "Integer")))

(define (exact? n) ;;FIXME: bogus
  (or (integer? n) (rational? n)))

(define (inexact? n) (not (exact? n)))

;; NOT r5rs
(define (reduce op seed list)
  (if (null? list) 
      seed
      (reduce op (op seed (car list)) (cdr list))
) )

;; NOT r5rs
(define (nary-and op seed list)
  (cond
   ((null? list) #t)
   ((op seed (car list)) (nary-and op (car list) (cdr list)))
   (else #f)
) )


(define (= x . others)
  (nary-and (lambda (x1 x2) (: x1 =  x2)) x others))

(define (< x . others)
  (nary-and (lambda (x1 x2) (: x1 <  x2)) x others))

(define (> x . others)
  (nary-and (lambda (x1 x2) (: x1 >  x2)) x others))

(define (<= x . others)
  (nary-and (lambda (x1 x2) (: x1 <= x2)) x others))

(define (>= x . others)
  (nary-and (lambda (x1 x2) (: x1 >= x2)) x others))


(define (zero?     z) (: z = 0))
(define (positive? x) (: x > 0))
(define (negative? x) (: x < 0))

(define (odd?  n) (: n odd))
(define (even? n) (: n even))

(define (max x . others)
  (reduce (lambda (x1 x2) (: x1 "max:" x2)) x others))

(define (min x . others)
  (reduce (lambda (x1 x2) (: x1 "min:" x2)) x others))

(define (+ . things)
  (reduce (lambda (x1 x2) (: x1 + x2)) 0 things))

(define (* . things)
  (reduce (lambda (x1 x2) (: x1 * x2)) 1 things))


;; NOT r5rs
(define (uncommunitive-reduce op seed things)
  (case (length things)
    ((0) seed)
    ((1) (op seed (car things)))
    ((2) (op (car things) (cadr things)))
    (else
     (reduce op
	     (op (car things) (cadr things)) 
	     (cddr things)))
) )

;; - and / are not commutive.
(define (- . things)
  (uncommunitive-reduce (lambda (x1 x2) (: x1 - x2)) 0 things))

(define (/ . things)
  (uncommunitive-reduce (lambda (x1 x2) (: x1 / x2)) 1 things))

;;(define (abs n) (if (: n < 0) (: 0 - n) n)) ;; fails for complex
(define (abs n) (: n "abs")) ;; use ST def (includes complex abs)

(define (quotient  n1 n2) (: n1 "quo:" n2))
(define (remainder n1 n2) (: n1 "rem:" n2))
(define (modulo    n1 n2)
  (let ( (rem (remainder n1 n2)) )
    (if (> n2 0)
	(if (< rem 0) (+ rem n2) rem)
	(if (> rem 0) (+ rem n2) rem))))


(define (gcd . numbers)
  (abs (uncommunitive-reduce (lambda (n1 n2) (: n1 "gcd:" n2)) 0 numbers)))

(define (lcm . numbers)
  (abs (uncommunitive-reduce (lambda (n1 n2) (: n1 "lcm:" n2)) 1 numbers)))


;; NOT r5r2
(define (factorial n)   (: n factorial))

(define (numerator   q) (: q numerator))
(define (denominator q) (: q denominator))

(define (floor    x) (: x floor))
(define (ceiling  x) (: x ceiling))
(define (truncate x) (: x truncated))
(define (round    x) ;; Smalltalk #rounded rounds to nearest
  ;; Scheme ROUND rounds to even
  (if (= 0.5 (: (: x "fractionPart") abs))
      (let ( (trunc (: x truncated)) )
	(if (even? trunc)
	    trunc
	    (if (negative? x)
		(- trunc 1)
		(+ trunc 1))))
      (if (negative? x)
	  (: (- x 0.5) truncated)
	  (: (+ x 0.5) truncated))
) )

; (rationalize x y) -- see file "ratize.scm"

(define (exp z)  (: z exp))
(define (log z)  (: z ln))
(define (sin z)  (: z sin))
(define (cos z)  (: z cos))
(define (tan z)  (: z tan))
(define (asin z) (: z "arcSin"))
(define (acos z) (: z "arcCos"))
(define (atan x) (: x "arcTan"))

;; (atan y x) = (angle (make-rectangular x y))
;;  = (asin (/ y (sqrt (+ (* x x) (* y y)))))
(define (atan y x)
  (if (null? x)
      (: y "arcTan")
      (: y "atan2:" x) ;;(: (: y / (: (: (: x * x) + (: y * y)) sqrt)) "arcSin")
) )

(define (sqrt z) (: z sqrt))

(define (expt z1 z2)
  (if (integer? z2)
      (: z1 "raisedToInteger:" z2)
      (: z1 "raisedTo:" z2)
) )

;; FIXME
(define (make-rectangular x1 x2) (: ($ "Complex") real: x1 imaginary: x2))
(define (make-polar x3 x4) (: ($ "Complex") magnitude: x1 angle: x2))
(define (real-part z) (: z real))
(define (imag-part z) (: z imaginary))
(define (magnitude z) (: z magnitude))
(define (angle z)     (: z angle))

;; NOT r5rs
(define (degrees->radians d) (: d "degreesToRadians"))
(define (radians->degrees r) (: r "radiansToDegrees"))

;; Scheme49 prints "0.0" as "0.".
(define (exact->inexact z) (+ z ($ "0.0")))
(define (inexact->exact z) (round z))

(define (number->string z . optional-radix) ;; @@FIXME: radix assumes a is integer.
  (let* ( (radix (if (null? optional-radix) 10 (car optional-radix))) 
	  (st-string
	   (case radix
	     ((2 8 16) (: z "radix:" radix))
	     ((10) (: z "asSchemeString"))
	     (else (error "Radix must be 2 8 10 or 16" radix))))
	)
    (case radix ;; trim, e.g., "16r100" to "100"
      ((2 8) (: st-string "copyFrom:" 3 to: (: st-string size)))
      ((10) st-string)
      ((16) (: st-string "copyFrom:" 4 to: (: st-string size)))
) ) )

;   (if (null? radix) 
;       (: z "asSchemeString")
;       (case (car radix)
; 	((2 8 10 16) (: z "radix:" radix))
; 	(else (error "number->string: unsupported radix" radix))
; ) )   )


; (define (string->number z . radix) ...) in "string2number.scm"

		;;   ---   E O F   ---   ;;

