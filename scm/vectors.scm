;; FILE: "vectors.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Vector Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VECTORS
;;;;;;;;;;

;; NOTE:  Scheme Vectors are Smallscript Lists.
(define (vector? obj) (: obj "isKindOf:" ($ "Array")))

(define (make-vector k . optional-obj)
  (let ( (fill (if (null? optional-obj) 
		   nil 
		   (car optional-obj)))
       )
    (: ($ "Array") "new:" k "withAll:" fill)
) )
;   (let ( (vec (: ($ "Array") new)) )
;     (let loop ( (count k) )
;       (if (<= count 0)
; 	  vec
; 	  (begin
; 	    (: vec add: obj)
; 	    (loop (- count 1)))))
; ) )

;; nary -- done by xlate
(define (vector . list) (list->vector list))

(define (vector-length vec) (: vec size))

(define (vector-ref vec k) 
  (: vec "at:" (+ 1 k)))

(define (vector-set! vec k obj)
  (: vec "at:" (+ 1 k) "put:" obj))

(define (vector->list vec) 
  (: vec "asRest")) ;; NB: don't convert elt-vecs to lists

(define (list->vector list) (: list "asArray")) 

(define (vector-fill! vec obj)
  (let loop ( (idx (- (vector-length vec) 1)) )
    (if (< idx 0)
	vec
	(begin
	  (vector-set! vec idx obj)
	  (loop (- idx 1))))
) )



		;;   ---   E O F   ---   ;;
