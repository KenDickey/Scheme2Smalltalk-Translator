;; FILE: xlate.scm
;; IMPLEMENTS: Translates basic Scheme expressions into
;;             SmallScript.
;; AUTHOR: Ken Dickey
;; DATE:   16 March 2002 -- SmallScript -> Squeak
;;	   05 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.


;; NOTES:
;;   Original DESUGAR from PLScheme Compiler by
;;     Jonathan Rees (now greatly transmorgified).
;;
;;   Does NOT require FORMAT.  Does use PRETTY-PRINT.
;;
;;   Output formatting makes some attempt to be readable but
;;     does NOT attempt pretty-printing.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE TRANSLATION
;;;;;;;;;;;;;;;;;;;


(define line-delimiter (integer->char 13)) ;; CR is ASCII 13 = #xD

(define (endline port) (display line-delimiter port))

(define (library-gen-header aux-string init-name port)
  (set! global-env-str "SmallScheme " )
  (endline port)
  (display "'# This file is encoded in ASCII" port)
  (endline port)
  (display "# line delimitor is CR." port)
  (endline port)
  (display "SmallScheme" port)
  (endline port)
  (endline port)
  (display "SmallScheme code for Squeak 3.4" port)
  (endline port)
  (display aux-string port)
  (endline port)
  (display "'!" port)
  (endline port)
  (endline port)
  (display "!SmallScheme class methodsFor: 'initialization'!" port)
  (endline port)
  (display init-name port) ;; e.g. "initBooleans"
)

(define (user-gen-header aux-string init-name port)
  (set! global-env-str "schemeEnv " )
  (endline port)
  (display "'# This file is encoded in ASCII" port)
  (endline port)
  (display "# line delimitor is CR." port)
  (endline port)
  (display "SmallScheme" port)
  (endline port)
  (endline port)
  (display "SmallScheme code for Squeak 3.0" port)
  (endline port)
  (display aux-string port)
  (endline port)
  (display "'!" port)
  (endline port)
  (endline port)
  (display "!SmallScheme methodsFor: 'user'!" port)
  (endline port)
  (display init-name port)
  (endline port) (endline port)
  (display "    \"Capture self as env for use by nested block code\"" port)
  (endline port)
  (display "    | schemeEnv | schemeEnv := self. " port)
  (endline port)
)

(define (gen-trailer port)
  (endline port)
  (display "! !" port)
  (endline port))

(define (xlate-from-file infile-name 
			 init-name 
			 gen-header gen-trailer . outport)
  (let ( (out (:optional outport (current-output-port))) )
    (call-with-input-file infile-name
      (lambda (in)
	;; header
	(gen-header (string-append "Translated from file " infile-name)
		    init-name
		    out)

        ;; xlate (body) forms
        (let loop ( (form (read in)) )
          (if (eof-object? form)
              'done
              (begin
                ;;(display #\" out)
                ;;(pretty-print form out) ;; @@@ needs " doubling
                ;;(display #\" out)
                (translate form out)
                (loop (read in)))))
        ;; footer
        (gen-trailer out)
) ) ) )

(define (make-init-name stem)
  (let ( (bashable (symbol->string (scheme->smalltalk-identifier stem))) )
    (string-set! bashable 0 (char-upcase (string-ref bashable 0)))
    (string-append "init" bashable)))


(define (xlate-library-file infile-name outfile-name stem)
  (call-with-output-file outfile-name
      (lambda (out)
        (xlate-from-file infile-name (make-init-name stem)
			 library-gen-header gen-trailer out))))


(define (make-instance-name stem) stem)

(define (xlate-file infile-name outfile-name stem)
  (call-with-output-file outfile-name
      (lambda (out)
        (xlate-from-file infile-name (make-instance-name stem)
			 user-gen-header gen-trailer out))))

;; scheme-code-string -> smalltalk-code-output-string-port
(define (scheme2smalltalk scheme-code-string output-port)
  (let ( (input-port (open-input-string scheme-code-string)) )
    (set! global-env-str "schemeEnv " )
    (display "[ :schemeEnv | " output-port)
    (translate (read input-port) output-port)
    (display "]" output-port)
    (endline output-port)
    output-port
) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSLATE a single expression form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (TRANSLATE scheme-exp . port)
  (let ( (outport (:optional port (current-output-port))) )
    (endline outport)
    (xlate (desugar scheme-exp 0) (make-empty-env) 0 outport)
    (display "." outport)
    (endline outport)
) )

(define global-env-str "SmallScheme " )

(define (global pre-str post-str) 
  (string-append pre-str global-env-str post-str))

;; NOTE: We need a compile-time environment to track
;; if a variable is local or global.
;; Local vars are scoped by Smalltalk.
;; Globals need a dereference operation.

(define (make-empty-env) '())
(define (extend-env new-vars env) (append new-vars env))
(define (is-global? sym locals-env) (not (memq sym locals-env)))


;; NOTE: symbols don't show up in XLATE as they have to
;; be quoted.  Non-quoted symbols are variable names or 
;; Scheme keywords.

;; Some forms (do, cond, case, ..) are "macro expanded"
;; away by DESUGAR, so XLATE does less.

(define (XLATE exp ct-env offset port)
  (cond ((empty-list? exp)
         (xlate-empty-list exp port))
        ((number? exp) 
         (xlate-number   exp port))
        ((boolean? exp)
         (xlate-boolean  exp port))
        ((string? exp)
         (xlate-string   exp port))
        ((char? exp)
         (xlate-char     exp port))
        ((quote? exp)
         (xlate-quote (cadr exp) offset port))
        ((variable? exp)
         (xlate->sts-variable    exp ct-env offset port))
        ((lambda? exp)
         (xlate->sts-lambda      exp ct-env offset port))
;;        ((case-lambda? exp)
;;         (xlate->sts-case-lambda exp ct-env offset port))
        ((define? exp)
         (xlate->sts-define      exp ct-env offset port))
        ((if? exp)
         (xlate->sts-if          exp ct-env offset port))
        ((begin? exp)
         (xlate->sts-begin       exp ct-env offset port))
        ((set!? exp)
         (xlate->sts-set!        exp ct-env offset port))
        ((named-let? exp)
         (xlate->sts-named-let   exp ct-env offset port))
        ((let? exp)
         (xlate->sts-let         exp ct-env offset port))
        ((let*? exp)
         (xlate->sts-let*        exp ct-env offset port))
        ((letrec? exp)
         (xlate->sts-letrec      exp ct-env offset port))
        ((dynamic-let? exp)
         (xlate->sts-dynamic-let    exp ct-env offset port))
        ((dynamic-define? exp)
         (xlate->sts-dynamic-define exp ct-env offset port))
        ((dynamic-ref-with-default? exp)
         (xlate->sts-dynamic-rwd    exp ct-env offset port))
        ((dynamic-ref? exp)
         (xlate->sts-dynamic-ref    exp ct-env offset port))
        ((dynamic-set!? exp)
         (xlate->sts-dynamic-set!   exp ct-env offset port))
	((values? exp)
	 (xlate->sts-values      exp ct-env offset port))
	((magic? exp)
	 (xlate-magically        exp ct-env offset port))
	((smalltalk-ref? exp) (emit-ss-ref exp port))
        ((application? exp)
         (xlate->sts-application exp ct-env offset port))
        (else (error "unknown expression type" exp))
) )


(define (:optional exp default)             
  (if (null? exp) default (car exp)))

(define (spaces n port)                 
  (let loop ((count n))                    
    (display #\space port)              
    (if (> n 0) (spaces (- count 1) port))))

(define (endline-and-spaces n port)
  (endline port)
  (spaces n port))


;; LAMBDA

(define lambda-body-indent 4)

;; (lambda (a b) (+ a b) <-> [:a :b| a + b]

;; (emit-build-rest-args foo offset port)
;; foo := foo asRest. -- foo is assumed to be an Array
(define (emit-build-rest-args rest-name offset port)
  (endline-and-spaces offset port)
  (display (scheme->smalltalk-identifier rest-name) port)
  (display " := " port)
  (display (scheme->smalltalk-identifier rest-name) port)
  (display " asRest." port)
  (endline-and-spaces offset port)
)

(define (lambda-has-rest? exp) 
  (not (list? (lambda-formals exp))))

(define (lambda-rest-and-other-formals exp) ;;-> (rest . others)
  (let ( (formals (lambda-formals exp)) )
    (if (symbol? formals)
	(cons formals '())
	(let loop ( (list formals) (others '()) )
	  (cond
	   ((null? list) 
	    (cons '() (reverse others))
	   )
	   ((not (pair? (cdr list))) 
	    (cons (cdr list) (reverse (cons (car list) others)))
	   )
	   (else (loop (cdr list) (cons (car list) others)))
        ) )
) ) )



(define (xlate->sts-lambda exp ct-env offset port)
  (let* ( (rest? (lambda-has-rest? exp))
	  (rest-and-others (lambda-rest-and-other-formals exp))
	  (rest-formal      (car rest-and-others))
	  (non-rest-formals (cdr rest-and-others))
	  (env-formals (if rest? 
			   (cons rest-formal non-rest-formals)
			   non-rest-formals))
	  (body-offset (+ offset lambda-body-indent))
        )
    (endline-and-spaces offset port)
    (display #\[ port)
    (if rest?
	(xlate-formals (append non-rest-formals (list rest-formal)) port)
	(xlate-formals non-rest-formals port))
    (if rest?
 	(emit-build-rest-args rest-formal body-offset port))
    ;; FIXME: optimize trivial rest case:
    ;; (lambda (x . one-arg) 
    ;;  (let ((whatever (if (null? one-arg default (car one-arg))))) ...)
    (map 
     (lambda (body-exp) 
       (xlate body-exp 
              (extend-env env-formals ct-env)
              body-offset 
              port))
     (lambda-body exp))
    (display #\] port)
    (if rest?
	(display " withLastArgRest " port))
) )

;; CASE-LAMBDA
; (define plus
;    (case-lambda 
;       (() 0)
;       ((x) x)
;       ((x y) (+ x y))
;       ((x y z) (+ (+ x y) z))
;       (args (apply + args))))
; =>
; R5RS.Scheme define: #plus as:
;  [ | args | := (varArgList asList).
;   (Switch new)
;   case: 0 do: ([0] value);
;   case: 1 do: ([:x| x] valueWithArguments args);
;   case: 2 do: ([:x :y| (x + y)] valueWithArguments args);
;   case: 3 do: ([:x :y :z| (x + y) + z] valueWithArguments args);
;   default: ((R5RS.Scheme at: #'+') valueWithArguments args);
;   on: (varArgList.size)].
;;
;; NB: if no rest, default is error.

(define (case-arity form) 
  (let loop ( (args (car form)) (len 0) )
    (cond
     ((null? args) len)
     ((not (pair? args)) (if (zero? len) 'nary (- len))
     )
     (else (loop (cdr args) (+ len 1)))
) ) )


(define (xlate->sts-case-lambda exp ct-env offset port)
  (let ( (case-offset (+ 10 offset)) 
	 (args-name (gensym "args"))
       )
    (display "[ | " port)
    (display args-name port)
    (display " | := (varArgList asList)." port)
    (endline-and-spaces offset port)
    (display " (Switch new)" port)
    (let loop ( (nargs-seen '()) (forms (cdr exp)) )
      (if (null? forms)
	  (if (not (memv 'nary nargs-seen))
	      (begin
		(endline-and-spaces offset port)
		(display "default: ((R5RS.Scheme globalRef: #'error') " port)
		(display "value: 'case-lambda: no matching case' value: " port)
		(display args-name port)
		(display ");" port)))
	  (let ( (form (car forms)) )
	    (endline-and-spaces offset port)
	    (let ( (arity (case-arity form)) )
	      (if (or (memv arity nargs-seen) 
		      (and (number? arity) 
			   (negative? arity) 
			   (memv 'nary nargs-seen)))
		  (error "case-lambda: arguments counts must be distinct"
			 exp))
	      (cond
	       ((eq? arity 'nary) ;; (rest body..)
		(display " default: (" port)
		(xlate->sts-lambda `(lambda ,(list (car form))
				      ,@(cdr form))
				   ct-env 
				   case-offset 
				   port)
		(display " value: ("  port)
		(display args-name port)
		(display " asRest));" port)
		(loop (cons arity nargs-seen) (cdr forms))
	       )
	       ((negative? arity) ;; nary ((a b . rest) body..)
		(display " default: (" port)
		(xlate->sts-lambda (cons 'lambda form) ct-env case-offset port)
		(endline-and-spaces case-offset port)
		(display " valueWithArguments: " port)
		(display args-name port)
		(display " );" port)
		(loop (cons arity (cons 'nary nargs-seen)) (cdr forms))
	       )
	       (else ;; exact ((a b c) body..)
		(display " case: " port)
		(display arity port)
	        (display " do: (" port)
		(xlate->sts-lambda (cons 'lambda form) ct-env case-offset port)
		(endline-and-spaces case-offset port)
		(display " valueWithArguments: " port)
		(display args-name port)
		(display " );" port)
		(loop (cons arity nargs-seen) (cdr forms))
	       )
    ) ) ) ) )
    (endline-and-spaces offset port)
    (display "on: (varArgList.size)]" port)
;;  (endline-and-spaces offset port)
) )

;; end xlate1.scm
