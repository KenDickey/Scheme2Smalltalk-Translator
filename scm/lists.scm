;; FILE: "lists.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Pair and List Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PAIRS and LISTS
;;;;;;;;;;;;;;;;;;

(define (pair? obj) (: obj "isPair"))

(define (cons obj1 obj2) (: ($ "Pair") car: obj1 cdr: obj2))
(define (car pair) (: pair car))
(define (cdr pair) (: pair cdr))

(define (set-car! pair obj)  (: pair "setCar:" obj))
(define (set-cdr! pair obj)  (: pair "setCdr:" obj))

(define (caar pair)   (: pair caar))
(define (cadr pair)   (: pair cadr))
(define (cddr pair)   (: pair cddr))
(define (cdar pair)   (: pair cdar))
(define (caaar pair)  (: pair caaar))
(define (caadr pair)  (: pair caadr))
(define (cadar pair)  (: pair cadar))
(define (cdaar pair)  (: pair cdaar))
(define (caddr pair)  (: pair caddr))
(define (cdadr pair)  (: pair cdadr))
(define (cdddr pair)  (: pair cdddr))
(define (cddar pair)  (: pair cddar))
(define (caaaar pair) (: pair caaadr))
(define (caaadr pair) (: pair caaadr))
(define (caadar pair) (: pair caadar))
(define (cadaar pair) (: pair cadaar))
(define (cdaaar pair) (: pair cdaaar))
(define (caaddr pair) (: pair caaddr))
(define (cadadr pair) (: pair cadadr))
(define (cdaadr pair) (: pair cdaadr))
(define (caddar pair) (: pair caddar))
(define (cdadar pair) (: pair cdadar))
(define (cadddr pair) (: pair cadddr))
(define (cdaddr pair) (: pair cdaddr))
(define (cddadr pair) (: pair cddadr))
(define (cdddar pair) (: pair cdddar))
(define (cddddr pair) (: pair cddddr))

(define (null? obj) 
  ;; (eq? obj '()) ;; (nil? obj)
  ;; Bummed for speed
  (: obj "==" ($ "nil")))

;; list? => proper list?
(define (list? obj) 
  (or (null? obj)
      (and (: obj "isKindOf:" ($ "Pair"))
	   (: obj "isProper"))))

;; nary: rewritten in xlate
(define (list . things) things)

(define (length list) (: list length))

;; NOT r5rs

(define (set-last-pair list thing)
  (cond 
   ((null? list) thing)
   ((not (pair? list))
    (error "set-last-pair: expected a list" list)
   )
   (else ;; at least 1 pair in list
    (let loop ( (result '()) (last-pair '()) (old list) )
      (if (or (null? old) (not (pair? old)))
	  (begin
	    (set-cdr! last-pair thing)
	    result)
	  (let ( (new (cons (car old) '())) )
	    (if (null? last-pair)
		(loop new new (cdr old))
		(begin
		  (set-cdr! last-pair new)
		  (loop result new (cdr old))))
	 ) )
   ))
) )

(define (append . lists)
 (if (null? lists)
     '()
     (let loop ((l lists))
	(if (null? (cdr l))
	    (car l)
	    (set-last-pair (car l) (loop (cdr l)))))))

;; NOT r5rs
(define (append! list1 list2) (: list1 "appendX21:" list2))

(define (reverse list) (: list reverse))

(define (list-tail list k)
  (if (<= k 0)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k) (car (list-tail list k)))

;; NOT r5rs
(define (last-pair list) (: list "lastPair"))

(define (set-last-pair! list thing)
  (set-cdr! (last-pair list) thing)
  list)

(define (memq   obj list) 
  (cond
   ((null? list) #f)
   ((eq? obj (car list)) list)
   (else
    (memq obj (cdr list)))
) )

(define (memv   obj list) 
  (cond
   ((null? list) #f)
   ((eqv? obj (car list)) list)
   (else
    (memv obj (cdr list)))
) )

(define (member obj list) 
  (cond
   ((null? list) #f)
   ((equal? obj (car list)) list)
   (else
    (member obj (cdr list)))
) )

(define (assq  obj alist)  
  (if (null? alist)
      #f
      (: alist detect: (lambda (pair) (eq?    obj (car pair)))))
)

(define (assv  obj alist)  
  (if (null? alist)
      #f
      (: alist detect: (lambda (pair) (eqv?   obj (car pair)))))
)

(define (assoc obj alist)  
  (if (null? alist)
      #f
      (: alist detect: (lambda (pair) (equal? obj (car pair)))))
)

;; NOT r5rs
(define (rassq  obj alist)  
  (if (null? alist)
      #f
      (: alist detect: (lambda (pair) (eq?    obj (cdr pair)))))
)


		;;   ---   E O F   ---   ;;
