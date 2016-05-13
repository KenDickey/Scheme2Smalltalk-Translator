;; FILE: "strings.scm"
;; IMPLEMENTS: Most of R^5RS Scheme String Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRINGS
;;;;;;;;;;

;; NB: Scheme 0 based, Smalltalk 1 based.

;; nary -- done by xlate
(define (string . rest)
  (if (null? rest) "" (list->string rest)))

(define (string? obj)
  (and (: obj "isKindOf:" ($ "String"))
       (not (: obj "isKindOf:" ($ "Symbol")))))


; (define (make-string k . optional-char)
;   (let ( (str (: ($ "String") new: k)) 
;          (fill (if (null? optional-char) 
; 		   #\space 
; 		   (car optional-char)))
;        )
;     (let loop ( (count k) )
;       (if (<= count 0)
; 	  str
; 	  (begin
; 	    (: str "at:" count "put:" fill)
; 	    (loop (- count 1)))))
; ) )

(define (make-string k . optional-char)
  (let ( (fill (if (null? optional-char) 
		   #\space 
		   (car optional-char)))
       )
    (: ($ "String") "new:" k "withAll:" fill)
) )

(define (string-length str) (: str size))

(define (string-ref str k) 
  (: str "at:" (+ 1 k)))

(define (string-set! str k char)
  (: str "at:" (+ 1 k) "put:" char))

(define (string=?  s1 s2) (: s1 =  s2))
(define (string<?  s1 s2) (: s1 <  s2))
(define (string>?  s1 s2) (: s1 >  s2))
(define (string>=? s1 s2) (: s1 >= s2))
(define (string<=? s1 s2) (: s1 <= s2))

(define (string-ci=?  s1 s2) (: (: s1 "asLowercase") =  (: s2 "asLowercase")))
(define (string-ci<?  s1 s2) (: (: s1 "asLowercase") <  (: s2 "asLowercase")))
(define (string-ci>?  s1 s2) (: (: s1 "asLowercase") >  (: s2 "asLowercase")))
(define (string-ci<=? s1 s2) (: (: s1 "asLowercase") <= (: s2 "asLowercase")))
(define (string-ci>=? s1 s2) (: (: s1 "asLowercase") >= (: s2 "asLowercase")))

(define (substring str start end)
  ; (unless (<= 0 start end (string-length s)) (error ..))
  (if (and (<= 0 start) 
	   (<= start end) 
	   (<= end (string-length str)))
      (: str "copyFrom:" (+ start 1) "to:" end )
      (error "substring: indexing error")))

(define (string-append . strings)
  (cond
   ((null? strings) "")
   ((null? (cdr strings)) (car strings))
   (else
      (reduce (lambda (s1 s2)  (: s1 "," s2)) (car strings) (cdr strings)))
) )

(define (string->list str) (: (: str "collectArray:" (lambda (c) c)) "asRest"))

(define (list->string list) 
  (if (null? list)
      ""
      (let ( (str (: "")) )
	(: list collect: (lambda (char) (: str ":=" str "," char "asString")))
	str)
) )

(define (string-copy str) (: str copy))

(define (string-fill! str char)
  (let loop ( (idx (- (string-length str) 1)) )
    (if (< idx 0)
	str
	(begin
	  (string-set! str idx char)
	  (loop (- idx 1))))
) )
    


		;;   ---   E O F   ---   ;;

