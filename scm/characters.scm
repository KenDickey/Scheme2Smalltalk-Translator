;; FILE: "characters.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Character Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHARACTERS
;;;;;;;;;;;;;

(define (char? obj) (: obj "isKindOf:" ($ "Character")))

(define (char=?  c1 c2) (: c1 =  c2))
(define (char<?  c1 c2) (: c1 <  c2))
(define (char>?  c1 c2) (: c1 >  c2))
(define (char>=? c1 c2) (: c1 >= c2))
(define (char<=? c1 c2) (: c1 <= c2))

(define (char-ci=?  c1 c2) (: (: c1 "asLowercase") =  (: c2 "asLowercase")))
(define (char-ci<?  c1 c2) (: (: c1 "asLowercase") <  (: c2 "asLowercase")))
(define (char-ci>?  c1 c2) (: (: c1 "asLowercase") >  (: c2 "asLowercase")))
(define (char-ci<=? c1 c2) (: (: c1 "asLowercase") <= (: c2 "asLowercase")))
(define (char-ci>=? c1 c2) (: (: c1 "asLowercase") >= (: c2 "asLowercase")))

;; NOTA BENE: [FIXME] much of the following assumes ASCII [FIXME]

(define (char-alphabetic? c)
  (or (char-upper-case? c)
      (char-lower-case? c)))

(define (char-numeric? c)
  (and (char>=? c #\0)
       (char<=? c #\9)))

(define (char-whitespace? c)
  (: (: c "asciiValue") <= 32)) ;; #x20=32=#\space
  ;;(memv c '(#\space #\newline ...)))

;; NB: NOT r5rs
(define (char-newline? c)
  (or (: c " asciiValue  =" 10) (: c " asciiValue =" 13))) ;; 10=lf, 13=cr

(define (char-upper-case? c)
  (and (char>=? c #\A)
       (char<=? c #\Z)))

(define (char-lower-case? c)
  (and (char>=? c #\a)
       (char<=? c #\z)))

(define (char->integer c) (: c "asciiValue"))
(define (integer->char n) (: n "asCharacter"))

(define (char-upcase   c) (: c "asUppercase"))
(define (char-downcase c) (: c "asLowercase"))


		;;   ---   E O F   ---   ;;

