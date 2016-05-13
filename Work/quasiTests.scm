;; Tested ok in Chez Scheme & Scheme48.

;; ,open signals
;; (load "D:/SmallScript/ProtoScheme/quasiTests.scm") ;;this file

(define (expect a b)
  (if (equal? a b)
      #t
      (warn "Not equal:" a b)))

(define (desugar exp level) 
  (if (and (pair? exp) (eq? (car exp) 'quote))
      (cdr exp)
      exp)) ;; test


; (define (last-pair list) 
;   (if (pair? list)
;       (let loop ( (rest list) )
; 	(if (pair? (cdr rest))
; 	    (loop (cdr rest))
; 	    rest))
;       list))

; (define (set-last-pair! list thing)
;   (if (null? list)
;       thing
;       (set-cdr! (last-pair list) thing))
;   list)

;; copy the list [scheme48 quoted lists are immutable]
(define (set-last-pair list thing)
  (cond 
   ((null? list) thing)
   ((not (pair? list))
    (error "set-last-pair: expected a list" list))
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
		  (loop result new (cdr old)))))
) ) )) )
       


(define (make-list-prefix-predicate sym)
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) sym))))

(define quote?
  (make-list-prefix-predicate 'quote ))

(define quasiquote?
  (make-list-prefix-predicate 'quasiquote ))

(define unquote?
  (make-list-prefix-predicate 'unquote ))

(define unquote-splicing?
  (make-list-prefix-predicate 'unquote-splicing ))

;; 'quasiquote, 'quote and 'unquote-splicing should not be 
;; used directly in uasiquote-templates

(define %quasiquote       ''quasiquote)
(define %unquote          ''unquote)
(define %unquote-splicing ''unquote-splicing)


;; (a ,(+ 1 2) ,@(map abs (-4 5)) 6) 
;;-> (cons 'a 
;;     (cons (+ 1 2) 
;;        (set-last-pair (map abs '(- 4 5)) 
;;           (cons 6 '()))))
;;=> (a 3 4 5 6)
;;
;; (a `(b ,(+ 1 2) `,(foo ,(+ 1 3) d) e) f)
;; -> (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

(define (rewrite-quasiquote exp level)
  ;; (quasiquote <whatever>)
  (if (= level 0)
      (build-quasi-list (cadr exp) (+ level 1))
      `(cons ,%quasiquote
             (cons 
              ,(build-quasi-list (cadr exp) (+ level 1))
              '() ))
) )

;; NB: assumes set-last-pair returns original list
(define (build-quasi-list thing level)
  (cond
   ((not (pair? thing)) `',thing)
   ((pair? (car thing))
    (let ( (sublist (car thing)) )
      (cond
       ((quasiquote? sublist)
        `(cons ,(rewrite-quasiquote sublist level)
               ,(build-quasi-list (cdr thing) level))
       )
       ((unquote? sublist)
        (if (= level 1)  ;; (zero? (- level 1))
            `(cons ,(desugar (cadr sublist) (- level 1))
                   ,(build-quasi-list (cdr thing) level))
            `(cons ,(build-quasi-list sublist level);;NB not level-1
                   ,(build-quasi-list (cdr thing) level))
       ))
       ((unquote-splicing? sublist)
        (if (= level 1)  ;; (zero? (- level 1))
            `(set-last-pair ,(desugar (cadr sublist) (- level 1))
                   ,(build-quasi-list (cdr thing) level))
            `(cons ,(build-quasi-list sublist (- level 1))
                   ,(build-quasi-list (cdr thing) level))
       ))
       (else 
        `(cons ,(build-quasi-list sublist level)
               ,(build-quasi-list (cdr thing) level))
      )) ;; end inner-cond
   ))
   ((eq? (car thing) 'unquote)
    (if (= level 1)
	(if (pair? (cdr thing))
	    (desugar (cadr thing) (- level 1))
	    (cdr thing))
        `(cons ,%unquote
               (cons
                ,(build-quasi-list (cadr thing) (- level 1))
                '() ))
   ))
   (else
    `(cons ',(car thing)
           ,(build-quasi-list (cdr thing) level))
) ))

(define (b  exp) (build-quasi-list   exp 1))
(define (rq exp) (rewrite-quasiquote exp 0))

(define (process thing)
  (eval thing (interaction-environment)))

(define (test exp)
  (process
   (rewrite-quasiquote exp 0)))

(expect (test '`(list ,(+ 1 2) 4)) 
	'(list 3 4))

(expect (test '`(+ 2 3))
	'(+ 2 3))

(expect (test '`(+ 2 ,(* 3 4)))
	'(+ 2 12))

(expect (test '`(a b (,(+ 2 3) c) d))
	'(a b (5 c) d))

(expect (test '`(a b ,(reverse '(c d e)) f g))
	'(a b (e d c) f g))

(expect (test '`(+ ,@(cdr '(* 2 3))))
	'(+ 2 3))
	      
(expect (test '`(a b ,@(reverse '(c d e)) f g))
	'(a b e d c f g))

;(expect (test ''`,(cons 'a 'b)) ;; needs desugar
;	'`,(cons 'a 'b))

(expect (test '`',(cons 'a 'b))
	''(a . b))

(define name 'a)
(expect (test '`(list ,name ',name))
	'(list a 'a))

(expect (test '`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
	'(a 3 4 5 6 b))

(expect (test '`((foo ,(- 10 3) ,@(cdr '(c))) . ,(car '(cons))))
	'((foo 7) . cons))

;(expect (test '`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
;	'#(10 5 2 4 3 8))

(expect (test '`(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
	'(10 5 2 4 3 8))

(expect (test '`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
	'(a `(b ,(+ 1 2) ,(foo 4 d) e) f))

(define name1 'x)
(define name2 'y)
(expect (test '`(a `(b ,,name1 ,',name2 d) e))
	'(a `(b ,x ,'y d) e))



                    ;;  ---  E O F  ---  ;;




