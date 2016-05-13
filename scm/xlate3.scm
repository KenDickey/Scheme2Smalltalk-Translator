;; xlate part 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESTRUCTURE, PREDICATE HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-list-prefix-predicate sym)
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) sym))))

(define quote?
  (make-list-prefix-predicate 'quote ))

(define lambda?
  (make-list-prefix-predicate 'lambda ))

(define case-lambda?
  (make-list-prefix-predicate 'case-lambda ))

(define set!?
  (make-list-prefix-predicate 'set! ))

(define let?
  (make-list-prefix-predicate 'let ))

(define let*?
  (make-list-prefix-predicate 'let* ))

(define letrec?
  (make-list-prefix-predicate 'letrec ))

(define dynamic-let?
  (make-list-prefix-predicate 'dynamic-let ))

(define dynamic-define?
  (make-list-prefix-predicate 'dynamic-define ))

(define dynamic-ref?
  (make-list-prefix-predicate 'dynamic-ref ))

(define dynamic-ref-with-default?
  (make-list-prefix-predicate 'dynamic-ref-with-default ))

(define dynamic-set!?
  (make-list-prefix-predicate 'dynamic-set! ))

(define quasiquote?
  (make-list-prefix-predicate 'quasiquote ))

(define unquote?
  (make-list-prefix-predicate 'unquote ))

(define unquote-splicing?
  (make-list-prefix-predicate 'unquote-splicing ))

;; (let name ((..)..) body..)
(define (named-let? exp) 
  (and (let? exp) 
       (symbol? (cadr exp))))

;;(define do?
;;  (make-list-prefix-predicate 'do ))

;;(define cond?
;;  (make-list-prefix-predicate 'cond ))

(define if?
  (make-list-prefix-predicate 'if ))

(define define?
  (make-list-prefix-predicate 'define ))

(define begin?
  (make-list-prefix-predicate 'begin ))

(define values?
  (make-list-prefix-predicate 'values ))

(define magic?
  (make-list-prefix-predicate ': ))

(define smalltalk-ref?
  (make-list-prefix-predicate '$ ))

(define (empty-list? thing) (eq? thing '() ))

(define keyword?
  (lambda (x)
    (member x '(quote lambda if begin letrec define))))

(define literal?
  (lambda (exp)
    (or (number? exp)
        (boolean? exp)
        (quote? exp))))

(define variable?
  (lambda (exp)
    (and (symbol? exp)
         (not (keyword? exp)))))

(define same-variable? eq?)

(define lambda-formals cadr)
(define lambda-body    cddr)

(define set!-target    cadr)
(define set!-value-exp caddr)

(define application?
  (lambda (exp)
    (and (pair? exp)
         (not (keyword? (car exp))))))

(define operator car)
(define operands cdr)

(define if-predicate  cadr)
(define if-consequent caddr)
(define if-alternate  cadddr)
(define (if-has-alternate? exp) ;; (if a b c)
  (not (null? (cdddr exp))))

(define begin-subexpressions cdr)

(define (definition-name form)
  (let ((pattern (cadr form)))
    (if (pair? pattern) (car pattern) pattern)))

(define (definition-body form)
  (let ((pattern (cadr form)))
    (if (pair? pattern)
        `(lambda ,(cdr pattern) ,@(cddr form))
        (caddr form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESUGAR SYNTAX (Scheme -> simplified Scheme)
;;;;;;;;;;;;;;;;;;

(define (map* fn . list) 	; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car list)) '())
   ((pair? (car list)) 
    (cons (apply fn      (map car list))
	  (apply map* fn (map cdr list))))
   (else  (apply fn list))))


;; Bootstrap macros before we have macros.

(define (desugar exp qq-level)
  (cond ((or (number? exp) (boolean? exp) (string? exp) (char? exp))
         exp)
        ((symbol? exp) exp)
        ((quote?  exp) exp)
        ((eq? exp '()) exp)
        ((lambda? exp)
         `(lambda ,(lambda-formals exp) 
	    ,(desugar-body (lambda-body exp) qq-level))
	)
	((define? exp)
	 `(define ,(definition-name exp)
	    ,(desugar (definition-body exp) qq-level))
	)
        ((set!? exp)
         `(set! ,(set!-target exp) 
		,(desugar (set!-value-exp exp) qq-level))
	)
        ((begin? exp)
         (desugar-body (begin-subexpressions exp) qq-level)
	)
        ((if? exp)
         `(if ,@(map (lambda (e) (desugar e qq-level)) (cdr exp)))
	)
        ((named-let? exp)
         `(let ,(let-name exp)
            ,(map (lambda (binding) 
		    `(,(let-formal binding) 
		      ,(desugar (let-init binding) qq-level)) )
                  (let-bindings exp))
            ,(desugar-body (let-body exp) qq-level))
	)
        ((memq (car exp) '(let let* letrec)) ;; same form
         `(,(car exp) 
            ,(map (lambda (binding) 
		    `(,(let-formal binding) 
		      ,(desugar (let-init binding) qq-level)) )
                  (let-bindings exp))
            ,(desugar-body (let-body exp) qq-level))
	)
        ((quasiquote? exp) 
	 (if (zero? qq-level)
	     (rewrite-quasiquote exp qq-level)
	     (desugar (rewrite exp) qq-level))
	)
	((sugar? exp)
	 (desugar (rewrite exp) qq-level)
	)
        (else  ;; application
	 (map* (lambda (e) (desugar e qq-level)) exp))
) )


;; (a ,(+ 1 2) ,@(map abs (-4 5)) 6) 
;;-> (cons 'a 
;;     (cons (+ 1 2) 
;;        (set-last-pair! (map abs '(- 4 5)) 
;;           (cons 6 '()))))
;;=> (a 3 4 5 6)
;;
;; (a `(b ,(+ 1 2) `,(foo ,(+ 1 3) d) e) f)
;; -> (a `(b ,(+ 1 2) ,(foo 4 d) e) f)

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
       
;; 'quasiquote, 'quote and 'unquote-splicing should not be 
;; used directly in uasiquote-templates

(define %quasiquote       ''quasiquote)
(define %unquote          ''unquote)
(define %unquote-splicing ''unquote-splicing)

(define (rewrite-quasiquote exp level)
  ;; (quasiquote <whatever>)
  (if (= level 0)
      (if (vector? (cadr exp))
	  `(list->vector 
	   ,(build-quasi-list (vector->list (cadr exp)) (+ level 1)))
	  (build-quasi-list (cadr exp) (+ level 1)))
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


(define (desugar-body body qq-level)
  (let loop ( (form  (car body)) 
	      (forms (cdr body)) 
	      (defines '())
	      (others  '())
	    )
    (cond
     ((define? form)
      (if (null? forms)
	  (error "expected body" body)
	  (loop (car forms) (cdr forms) (cons form defines) others))
     )
     ((null? forms)
      (if (null? defines) ;; just process the body
	  (let ( (body (map 
			(lambda (exp) (desugar exp qq-level)) 
			body))
	       )
	    (if (null? (cdr body))
		(car body)
		`(begin ,@body))
	  )
	  ;; else internal defines -> letrec
	  `(letrec ,(map (lambda (def) 
			   (list (definition-name def)
				 (desugar (definition-body def) qq-level)))
			 (reverse defines))
	     ,(desugar-body (reverse (cons form others)) qq-level))
       ))
     (else
      (loop (car forms) (cdr forms) defines (cons form others)))
) ) )


;; "Syntactic sugar causes cancer of the semicolon." Alan Perlis.

(define (SUGAR? exp)
  (and (pair? exp)
       (member (car exp) 
	       '(and or cond do case delay string vector)))) ;; list

;; let let* handled by xlate, above.

(define unspecified "Unspecified")


;; REWRITE

(define (REWRITE exp)
  (cond ((not (pair? exp))           exp)
        ((eq? (car exp) 'and)        (rewrite-and    exp))
        ((eq? (car exp) 'or)         (rewrite-or     exp))
        ((eq? (car exp) 'cond)       (rewrite-cond   exp))
        ((eq? (car exp) 'do)         (rewrite-do     exp))
        ((eq? (car exp) 'case)       (rewrite-case   exp))
        ((eq? (car exp) 'delay)      (rewrite-delay  exp))
;;      ((eq? (car exp) 'list)       (rewrite-list   exp))
        ((eq? (car exp) 'string)     (rewrite-string exp))
        ((eq? (car exp) 'vector)     (rewrite-vector exp))
        ;; FIXME: other nary procs? ('+' et al)
        (else exp)))


;; AND

(define (rewrite-and exp)
  (let ( (conjuncts (cdr exp)) )
    (cond 
     ;; (and) -> #t
     ((null? conjuncts) #t)
     ;; (and <test>) -> <test>
     ((null? (cdr conjuncts)) 
      (car conjuncts))
     ;; (and <test1> <test2> ...) 
     ;;-> (if <test1> (and <test2> ...) #f)      
     (else 
      `(if ,(car conjuncts)
	   (and ,@(cdr conjuncts))
	   #f)))
) )


;; OR

(define (rewrite-or exp)
  (let ((disjuncts (cdr exp)))
    (cond 
     ;; (or) -> #f
     ((null? disjuncts) `#f)
     ;; (or <test>) -> <test>
     ((null? (cdr disjuncts)) (car disjuncts))
     ;; ((or <test1> <test2> ...) 
     ;;-> (let ( (temp <test1>) )
     ;;      (if temp temp (or <test2> ...))
     (else 
      (let ( (temp   (gensym "temp"))
	     (test   (car disjuncts))
	     (others (cdr disjuncts))
	   )
	`(let ( (,temp ,test) )
	   (if ,temp
	       ,temp
		(or ,@others))))))
) )


;; COND 

(define (rewrite-cond exp)
  (let ( (clauses (cdr exp)) )
    (cond 
     ;; (cond)
     ((null? clauses) 
      `',unspecified 
     )
     ;; cond (<test>) <clause> ...)
     ;;-> (or <test> (cond <clause> ...))
     ((null? (cdar clauses))
      `(or ,(caar clauses)
	   (cond ,@(cdr clauses)))
     )
     ;; (cond (<test> => <recipient>) <clause> ...)
     ;;=>
     ;; (let ( (result <test>) )
     ;;    (if result
     ;;       (<recipient> result)
     ;;       (cond <clause> ...))
     ((eq? (cadar clauses) '=>)
      (let ( (result    (gensym 'result)) 
	     (test      (caar   clauses))
	     (recipient (caddar clauses))
	     (others    (cdr    clauses))
           )
	`(let ( (,result ,test) )
	   (if ,result
	       (,recipient ,result)
	       (cond ,@others))))
     )
     ;; (cond (else <exp1> <exp2> ...)) 
     ;;-> (begin <exp1> <exp2> ...)
     ((eq? (caar clauses) 'else)
      `(begin ,@(cdar clauses))
     )
     ;; (cond (<test> <exp1> <exp2> ...) <clause> ...)
     ;;-> (if <test> (begin <exp1> <exp2> ...) (cond <clause> ...))
     (else `(if ,(caar clauses)
		(begin ,@(cdar clauses))
		(cond ,@(cdr clauses)))))))


;; DO 

; (do ( (var init step) ..) 
;     (exit-test exp ..)
;     command..)
;->
; (let <gensym> ((var init)..)
;   (if exit-test
;       (begin exp..)
;       (begin
;          command..
;          (<gensym> step..))))

(define (rewrite-do exp)
  (let ( (loop-name (gensym "do-loop"))
	 (locals    (cadr  exp))
	 (exits     (caddr exp))
	 (commands  (cdddr exp))
       )
    (let ( (var-inits 
	    (map (lambda (vis) (list (car vis) (cadr vis))) 
		 locals))
	   (steps 
	    (map (lambda (vis) 
		   (if (null? (cddr vis)) (car vis) (caddr vis))) 
		 locals))
	   (test  (car exits))
	   (exps  (cdr exits))
	 )
      `(let ,loop-name ,var-inits
	    (if ,test
		(begin ,@exps)
		(begin
		  ,@commands
		  (,loop-name ,@steps))))
) ) )


;; CASE

(define (rewrite-case exp)
  (let ( (clauses (cdr exp)) )
    (cond ((null? clauses)
	    `',unspecified
	  )
	  ; (case <key>) -> <key>
	  ((null? (cdr clauses))
	   (car clauses)
	  )
	  ; (case <key> (else <exp1> <exp2> ...))
          ; -> (begin <key> <exp1> <exp2> ...)
          ((eq? (caadr clauses) 'else)
           `(begin ,(car clauses) ,@(cdadr clauses))
	  )
	  ;  (case <key> ((<datum> ...) <exp1> <exp2> ...) <clause> ...)
	  ;=>
          ; (let ( (key <key>) )
          ;    (if (memv key '(<datum> ...))
          ;      (begin <exp1> <exp2> ...)
          ;      (case key <clause> ...)))
          (else 
	   (let ( (key-name (gensym 'key)) 
		  (key      (car   clauses))
		  (datums   (caadr clauses))
		  (exps     (cdadr clauses))
		  (others   (cddr  clauses))
                )
	     `(let ( (,key-name ,key) )
		(if (memv ,key-name ',datums)
                     (begin ,@exps)
                     (case ,key-name ,@others))))))
) )


;; DELAY 

(define (rewrite-delay exp)
  `(make-promise (lambda () ,(cadr exp))))


;; The following are needed until we get nary up..

;; LIST

(define (rewrite-list exp)
  (if (null? (cdr exp))
      ''()
      `(cons ,(cadr exp) (list ,@(cddr exp)))))

(define (rewrite-string exp)
  `(list->string (list ,@(cdr exp))))

(define (rewrite-vector exp)
  `(list->vector (list ,@(cdr exp))))


                ;;; --- E O F --- ;;;

