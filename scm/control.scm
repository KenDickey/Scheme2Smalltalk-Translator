;; FILE: "control.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Control Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTROL FEATURES
;;;;;;;;;;;;;;;;;;;

(define (procedure? obj) 
  (or (: obj "isKindOf:" ($ "BlockClosure"))
      (: obj "isKindOf:" ($ "Continuation"))))


; (apply proc arg1 ... arglist) procedure
;   Proc must be a procedure and arglist must be a list. 
;   Calls proc with the elements of the list
;    (append (list arg1 ...) arglist)
;   computed and returned. 

(define (apply proc . args)
  (case (length args)
   ((0) (proc))
   ((1) (let ( (arg (car args)) )
	  (if (: arg "==" ($ nil))
	      (proc) ;; [nil asArray] => #().
	      (: proc "valueWithArguments:" (: arg "asArray"))))
   )
   (else
    (let* ( (lp (last-pair args))
	    (arglist (car lp))
	  )
      (if (not (list? arglist))
	  (error "APPLY requires a list as the last argument" args))
      (set-cdr! lp (cdr arglist))
      (set-car! lp (car arglist))
      (: proc "valueWithArguments:" (: args "asArray")))
) ))

(define (map fn . list)
  (cond
   ((null? (car list)) '())
   ((pair? (car list)) 
    (cons (apply fn     (: list collect: car))
	  (apply map fn (: list collect: cdr)))
   )
   (else  (apply fn list))
) )

;; NOT r5rs
(define (any pred list)
  (cond
   ((null? list) #f)
   ((pred (car list)) (car list))
   (else (any pred (cdr list)))
) )


(define (for-each proc first . rest)
  (cond
   ((null? rest)
    (if (not (null? first))
	(: first do: proc))
   )
   (else
      (let loop ( (lists (cons first rest)) )
	(if (not (any null? lists))
	    (begin
	      (apply proc (: list collect: car))
	      (loop  proc (: list collect: cdr)))
      ) )
) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DELAY and PROMISE
;;;;;;;;;;;;;;;;;;;;

; (define-syntax delay
;   (syntax-rules ()
;     ((delay ?exp) (make-promise (lambda () ?exp)))))

(define (make-promise thunk)
  (let ( (forced? #f) (result #f) )
    (lambda ()
      (if forced?
          result
          (let ( (whatever (thunk)) )
            (if forced?  ;; NB: a promise may be self-referential.
                result
                (begin
                  (set! result  whatever)
                  (set! forced? #t)
                  (set! thunk   #f) ;; for GC
                  result)))
) ) ) )

(define (force promise) (promise))

;; FIXME
(define (call-with-current-continuation proc) (: proc "callCC"))
(define call/cc call-with-current-continuation)

;; (values ...) returns a Smallscript List (xlate rewrite).
(define (call-with-values producer consumer)
  (: consumer "valueWithArguments:" (producer)))

; FIXME: bogus
(define (dynamic-wind before thunk after)
  (: (lambda () (before) (thunk)) ensure: (lambda () (after))))


;; FIXME: Hey, namespaces!
;; For the following include "eval.scm")
; (eval exp env-spec)
; (scheme-report-environment version)
; (null-environment version)
; (interaction-environment)


		;;   ---   E O F   ---   ;;

