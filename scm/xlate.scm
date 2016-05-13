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
  (display "SmallScheme code for Squeak 3.0" port)
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

(define (scheme2smalltalk scheme-code-string output-port)
  (let ( (input-port (open-input-string scheme-code-string)) )
    (set! global-env-str "schemeEnv " )
    (endline output-port)
    (display "[ :schemeEnv | " output-port)
    (endline output-port)
    (translate (read input-port) output-port)
    (endline output-port)
    (display "]" output-port)
    (endline output-port)
    (get-output-string output-port) ;; result is a string
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


;; DEFINE 

(define define-indent 6)

(define (xlate->sts-define exp ct-env offset port)
  (endline-and-spaces offset port)
  (display (global  ""  "define: " ) port)
  (let ( (def-name (definition-name exp)) )
    (xlate-symbol def-name port)
    (if (not (equal? def-name (scheme->smalltalk-identifier def-name)))
	(xlate-symbol-as-st-comment def-name port))
    (endline-and-spaces offset port)
    (display " as: ( " port)
    (xlate (definition-body exp) ct-env (+ offset define-indent) port)
    (display " )" port)
    (endline-and-spaces offset port))
)

;; VARIABLEs

;; Lexical lookups are done by Smalltalk.
;; Globals must be looked up in the proper Scheme namespace.

(define (xlate->sts-variable variable-name ct-env offset port)
  ;; (ignore offset)
  (display #\space port)
  (if (not (is-global? variable-name ct-env))
      (display (scheme->smalltalk-identifier variable-name) port)
      (begin
	(xlate-global-ref variable-name port)
	(endline-and-spaces (+ 4 offset) port)))
)

(define (xlate-symbol-as-st-comment sym port)
  (display "    \"" port) ;; add some prefix whitespace
  (for-each 
   (lambda (char)
     (display char port)
     ;;Smalltalk doubles #\! in code files
     (if (char=? char #\!)
	 (display char port)))
   (string->list (symbol->string sym)))
  (display "\"" port))
  
;; FIXME: born slow, conses a lot
(define scheme->smalltalk-identifier 
  ;; Could (string->list "!$%&*+-./:<=>?@^_~") but some compilers 
  ;; gen better code w quoted constant.
  (let ( (specials '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/ 
		     #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~ )) )
    (lambda (name) ; name is a string or symbol
      ;; X -> XX, special -> Xhh, where hh is ASCII hex value.
      (let loop ( (result '()) 
		  (chars (string->list 
			  (if (symbol? name)
				       (symbol->string name)
				       name)))
	        )
	(if (null? chars)
	    (string->symbol (list->string (reverse result)))
	    (let ( (char (car chars)) )
	      (cond
	       ((memq char specials)
		(loop (add-reverse-hex-chars char result)
		      (cdr chars))
	       )
	       ((char=? char #\X) ;; "byte-stuff x"
		(loop (cons #\X (cons #\X result))
		      (cdr chars))
	       )
	       (else
		(loop (cons char result) (cdr chars))))))
) ) ) )


(define (add-reverse-hex-chars char result)
  (let* ( (chars (string->list 
		  (number->string 
		   (char->integer char)
		   16))) 
       )
    (append (reverse (cons #\X chars)) result)))



;; CONSTANTS/LITERALS

(define (xlate-number number port)
  ;; FIXME xform syntax
  (display #\space port)
  ;; prefix, e.g. ".5" with "0"
  (if (< (abs number) 1)
      (display "0" port))
  (display number port))

(define (xlate-string string port)
  ;; FIXME xform syntax
  (display " '" port)
  (for-each
   (lambda (char)
     (display char port)
     ;; Smalltalk doubles #\' and #\` in strings.
     ;; Smalltalk source code uses #\!, so double as well
     (if (char=? char #\')
	 (display char port)
	 (if (char=? char #\`)
	     (display char port)
	     (if (char=? char #\!)
		 (display char port))))
     )
   (string->list string))
  (display "'" port)
)

(define (xlate-char char port)
  (if (eq? char #\newline)
      (display "(Character cr)" port)
      (begin
	(display " $" port)
	(display char port)
	;; double #\! in Smalltalk source code
	(if (char=? char #\!)
	    (display char port)))
) )

(define (xlate-boolean boolean port)
  (if boolean
      (display " true" port)
      (display " false" port)))

(define (xlate-empty-list exp port)
  (display " nil" port))

(define (xlate-symbol sym port)
  (display " #'" port)
  (display (scheme->smalltalk-identifier sym)   port)
  (display #\'   port))

(define (xlate-global-ref name port)
  (display (global  "("  "globalRef: " ) port)
  (xlate-symbol name port)
  (display ")" port))

;; (a b) -> ":a :b | "
(define (xlate-formals formals port)
  (if (> (length formals) 0)
      (begin
	(for-each 
	 (lambda (name)
	   (display " :" port)
	   (display (scheme->smalltalk-identifier name) port))
	 formals)
	(display " | " port))))

(define (xlate-quote exp offset port)
  ;; FIXME: pretty-print based on size and offset
  (cond ((symbol?  exp) (xlate-symbol  exp port))
        ((number?  exp) (xlate-number  exp port))
        ((boolean? exp) (xlate-boolean exp port))
        ((string?  exp) (xlate-string  exp port))
        ((char?    exp) (xlate-char    exp port))
        ((empty-list? exp) (display " nil" port))
        ((list? exp)
            (display " ({" port)
            (for-each 
	     (lambda (thing)
	       (xlate-quote thing offset port)
	       (display ". " port))
	     exp)
            (display "} asRest)" port)
	    )
	((pair?   exp)
	 (display " (Pair car: " port)
	 (xlate-quote (car exp) offset port)
	 (display " cdr: " port)
	 (xlate-quote (cdr exp) offset port)
	 (display ")" port)
	)
	((vector? exp)
	 (display " {" port)
	 (for-each
	  (lambda (thing)
	    (xlate-quote thing offset port)
	    (display ". " port))
	  (vector->list exp))
	 (display "}" port) ;; "." ??
	 )
       (else (error "unknown quoted expression type" exp))
) )


;; IF

;; (if <pred> <conseq> <alt>)   -> if <pred> <conseq> else <alt>.
;; (if <pred> <conseq>)         -> if <pred> <conseq>.

(define if-indent 4)

;;NB: 0 == false in Smallscript!
;; (if x ...) => (if ((x == false) not)...)

(define (xlate->sts-if exp ct-env offset port)
  ;; FIXME: formatting
  (let ( (if-offset (+ offset if-indent)) )
    (display " (((" port)
    (xlate (if-predicate exp) ct-env if-offset port)
    (display ") == false) not)" port)
    (endline-and-spaces if-offset port)
    (display "	ifTrue: [" port)
    (xlate (if-consequent exp) ct-env if-offset port)
    (display #\] port)
    (if (if-has-alternate? exp)
      (begin
        (endline-and-spaces if-offset port)
        (display "	ifFalse: [" port)
        (xlate (if-alternate exp) ct-env if-offset port)
        (display #\] port)))
) )


;; BEGIN

(define begin-indent 2)

;; FIXME: coalesce nested BEGINs
(define (xlate->sts-begin exp ct-env offset port)
  (let ( (new-offset (+ offset begin-indent)) )
    (display " [" port)
    (map (lambda (exp)
           (endline-and-spaces new-offset port)
           (xlate exp ct-env new-offset port)
           (display #\. port))
     (begin-subexpressions exp))
    (endline-and-spaces offset port)
    (display " ] value" port)))

;; SET!

(define set!-var cadr)

(define set!-val caddr)

(define (xlate->sts-set! exp ct-env offset port)
  (let ( (var (set!-var exp))
         (val (set!-val exp))
       )
    (cond 
     ((and (pair? var) ;; ($ name) -> name
	   (eq? (car var) '$))
      (display (global  "("  "setX21: " ) port)
      (display (cadr var) port)
      (display " as: " port)
      )
     ((is-global? var ct-env)
      ;; global assignment
      (display (global  "("  "setX21: " ) port)
      (xlate-symbol var port)
      (display " as: " port)
     )
     (else ;; local assignment
      (display "(" port)
      (display (scheme->smalltalk-identifier var) port)
      (display " := " port)
      )
    )
    (xlate val ct-env offset port)
    (display ")" port)
) )


;; LET, LET*, LETREC

(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))


(define let-formal car)
(define let-init  cadr)

(define (let-formals exp)
  (map let-formal (let-bindings exp)))


(define (let-inits exp)
  (map let-init (let-bindings exp)))


(define let-name cadr)

;; (let ( (var init).. ) body..)
;;->
;; ((lambda (var ..) body..) init..)
;;  -> [:var.. |
;;         body..
;;     ] value: (init..).

(define let-body-indent 4)
(define let-indent 2)

(define (xlate->sts-let exp ct-env offset port)
  (let ( (body-offset (+ offset let-body-indent)) 
         (formals (let-formals exp))
	 (inits (let-inits exp))
       )
    (display " [" port)
    (xlate-formals formals port)
    (map 
      (lambda (body-exp)
        (endline-and-spaces body-offset port)
        (xlate body-exp (extend-env formals ct-env) body-offset port)
        (display #\. port))
      (let-body exp))
    (endline-and-spaces offset port)
    (display "] " port)
    (if (null? inits)
	(display "value" port)
	(begin
	  (display "valueWithArguments: {" port)
	  (map 
	   (lambda (init)
	     (display "(" port)
	     (xlate init ct-env body-offset port)
	     (display "). " port))
	   inits)
	  (display "}" port)))
) )


;; (let loop ((n n) (a 1))
;;   (if (< n 2) a (loop (- n 1) (* n a))))
;; ->
;;   | loop | 
;;	loop := 
;;         [:n :a |
;;           if (n < 2) 
;;              then [a] 
;;              else [loop :value (n - 1) value: (n * a)]].
;;   loop valueWithArguments: {n. 1.}.

(define named-let-indent 4)

;; Note: I put the named let in a block because in some
;; value contexts the desired form is a single statement.
(define (xlate->sts-named-let exp ct-env offset port)
  (let ( (new-offset (+ offset named-let-indent)) 
         (new-env (extend-env 
                     (cons (let-name exp) (let-formals exp))
                     ct-env))
	 (tag-name (scheme->smalltalk-identifier (let-name exp)))
       )
    ;; locals
    (endline-and-spaces offset port)
    (display " [ | " port)
    (display tag-name port)
    (display " | " port)
    (endline-and-spaces (+ offset 4) port)
    (display tag-name port)
    (display " := " port)
    ;; args
    (endline-and-spaces new-offset port)
    (display "[" port)
    (xlate-formals (let-formals exp) port)
    ;; body
    (for-each
     (lambda (body-exp)
       (endline-and-spaces new-offset port)
       (xlate body-exp new-env new-offset port))
     (let-body exp))
    (endline-and-spaces offset port)
    (display "]." port)
    ;; invocation
    (endline-and-spaces offset port)
    (display tag-name port)
    (display " valueWithArguments: {" port)
    (for-each (lambda (init)
                (endline-and-spaces new-offset port)
                (xlate init new-env new-offset port)
                (display ". " port))
              (let-inits exp))
    (display "} ] value" port)
) )
    

;; (let* ( (a 1) (b (+ a 2)) ) body ..)
;; ->
;; ((lambda (a) 
;;     (lambda (b) body..) (+ a 2)) 1)
;; -> [:a | [:b | body..] value: (a+1.)] value: (1)
;;
;; i.e.
;;  [:v1 |
;;     [:v2 | 
;;         body..
;;     ] value: (init2)
;;  ] value: (init1)

(define (xlate->sts-let* exp ct-env offset port)
  (let loop ( (formals (let-formals exp))
              (inits   (let-inits   exp))
              (body-offset offset)
              (env ct-env)
            )
    (if (null? formals)
        (map ;; emit body expression(s)
         (lambda (body-exp)
           (endline-and-spaces body-offset port)
           (xlate body-exp env body-offset port))
         (let-body exp))
        (let ( (formal (list (car formals))) ;; do 1 binding
               (init   (car inits))
             )
          (endline-and-spaces body-offset port)
          (display "[" port)
          (xlate-formals formal port)
          (loop (cdr formals) 
             (cdr inits) 
             (+ body-offset let-indent)
             (extend-env formal env))
          (endline-and-spaces body-offset port)
          (display "] value: (" port)
          (xlate init env body-offset port)
          (display ")" port)))
) )


;;(letrec ( (a <a>) (b <b>) ..) body..)
;;      ->
;;      [ a b .. |
;;        a := <a>.
;;        b := <b>.
;;          ..
;;        body..
;;      ] value.

(define (xlate->sts-letrec exp ct-env offset port)
  (let* ( (body-offset (+ offset let-body-indent)) 
          (init-offset (+ offset let-indent))
          (formals (let-formals exp))
          (inits   (let-inits   exp))
          (new-env (extend-env formals ct-env))
       )
    (endline-and-spaces offset port)
    (display " [ |" port)
    (for-each 
     (lambda (formal)
       (display " " port)
       (display (scheme->smalltalk-identifier formal) port))
     formals)
    (display " |" port)
    (endline-and-spaces offset port)
    (map
     (lambda (name init)
       (endline-and-spaces init-offset port)
       (xlate->sts-variable name new-env offset port)
       (display " := (" port)
       (xlate init new-env init-offset port)
       (display ")." port)
       )
     formals
     inits)
    (map 
      (lambda (body-exp)
        (endline-and-spaces body-offset port)
        (xlate body-exp new-env body-offset port)
        (display #\. port))
      (let-body exp))
    (endline-and-spaces offset port)
    (display "] value" port)
) )

; DYNAMIC-LET  -- like LET, but dynamic/fluid vars in own namespace

;; (dynamic-let ( (var init).. ) body..)
;;->
;; [dynamicPush: var as: init. 
;;  ..
;;  body..] ensure: [dynamicPop: var. ...].

(define (xlate->sts-dynamic-let exp ct-env offset port)
  (let ( (body-offset (+ offset let-indent)) )
    (display " [" port)
    (map 
     (lambda (binding) 
       (endline-and-spaces body-offset port)
       (display (global  ""  "dynamicPush: #" ) port)
       (display (scheme->smalltalk-identifier (let-formal binding)) port)
       (endline-and-spaces (+ 10 body-offset) port)
       (display " as: " port)
       (xlate (let-init binding) ct-env body-offset port)
       (display #\. port))
     (let-bindings exp))
    (map 
     (lambda (body-exp)
       (endline-and-spaces body-offset port)
       (xlate body-exp ct-env body-offset port)
       (display #\. port))
     (let-body exp))
    (endline-and-spaces offset port)
    (display "] ensure: [" port)
    (map 
      (lambda (formal)
	(endline-and-spaces body-offset port)
	(display (global  ""  "dynamicPop: #" ) port)
	(display (scheme->smalltalk-identifier formal) port)
	(display #\. port))
      (let-formals exp))
    (display "]." port)
    (endline port)))

;; DYNAMIC-DEFINE var val
(define (xlate->sts-dynamic-define exp ct-env offset port)
  (endline-and-spaces offset port)
  (display (global  ""  "dynamicDefine: #" ) port)
  (display (scheme->smalltalk-identifier (definition-name exp)) port)
  (endline-and-spaces offset port)
  (display " as: " port)
  (xlate (definition-body exp) ct-env (+ offset define-indent) port)
)


;; DYNAMIC-REF val

(define (xlate->sts-dynamic-ref    exp ct-env offset port)
  (display (global  "("  "dynamicRef: #" ) port)
  (display (scheme->smalltalk-identifier (cadr exp)) port)
  (display ")" port))

;; dynamic-ref-with-default var thunk
(define (xlate->sts-dynamic-rwd    exp ct-env offset port)
  (display (global  "("  "dynamicRef: #" ) port)
  (display (scheme->smalltalk-identifier (cadr exp)) port)
  (display " withDefault: " port)
  (xlate (caddr exp) ct-env offset port)
  (display ")" port))

;; DYNAMIC-SET! var val
(define (xlate->sts-dynamic-set!   exp ct-env offset port)
  (let ( (var (set!-var exp))
         (val (set!-val exp))
       )
    (display (global  "("  "dynamicSetX21: #" ) port)
    (display (scheme->smalltalk-identifier var) port)
    (display " as: " port)
    (xlate val ct-env offset port)
    (display ")" port)
) )


;; APPLICATION

(define (xlate->sts-application exp ct-env offset port)
  (let ( (op (operator exp)) )
    (display "(" port)
    ;; operator
    (if (variable? op)
        (if (is-global? op ct-env)
            (begin
	     (xlate-global-ref op port)
	     (endline-and-spaces (+ 4 offset) port))
            (display (scheme->smalltalk-identifier op) port))
        (begin
          (display "(" port)
          (xlate op ct-env offset port)
          (display ")" port)))
    ;; operands
    (let* ( (operands     (operands exp)) 
            (num-operands (length operands))
          )
        (cond  ;; FIXME: use CASE after REWRITE-CASE is written
          ((zero? num-operands) 
           (display " value)" port)
          )
          ((= 1 num-operands) 
           (display " value: (" port)
           (xlate (car operands) ct-env offset port)
           (display "))" port)
          )
          ((= 2 num-operands) 
           (display " value: (" port)
           (xlate (car operands) ct-env offset port)
           (display ") value: (" port)
           (xlate (cadr operands) ct-env offset port)
           (display "))" port)
          )
          ((= 3 num-operands) 
           (display " value: (" port)
           (xlate (car operands) ct-env offset port)
           (display ") value: (" port)
           (xlate (cadr operands) ct-env offset port)
           (display ") value: (" port)
           (xlate (caddr operands) ct-env offset port)
           (display "))" port)
          )
          (else 
            (endline-and-spaces offset port)
            (display " valueWithArguments: {" port)
            (for-each 
             (lambda (whatever)
               (display #\space port)
               (xlate whatever ct-env offset port)
               (display #\. port))
             operands)
            (display "})" port)
      ) ) )
) )


;; VALUES

;; Smalltalk already has a form of multiple values.
;; Just make the values into a SS List.
(define (xlate->sts-values exp ct-env offset port)
  (display " {" port)
  (for-each 
   (lambda (whatever)
     (display #\space port)
     (xlate whatever ct-env offset port)
     (display #\. port))
   (cdr exp))
  (display "}" port))


;; MAGIC (-> Smalltalk)

;; Low level magic to make writing Smalltalk "primops" easy.
;; Nota Bene: Use strings for mixed-case selectors
;;            because (symbol->string 'asString) -> "asstring".

;; (: "Hello" "asString" )
;; ->
;; ( 'Hello' asString)

(define (xlate-magically exp ct-env offset port)
  ;; (: obj selector obj2 sel2 ... objN)
  ;; Must have at least 1 selector (min length is 3).
  ;; May end in selector or object.
  (display " (" port)
  (let loop ( (stuff (cdr exp)) (odd? #t) ) ; (obj sel ..)
      (if (null? stuff)
	  unspecified
	  (begin
	    (if odd?
		(xlate (car stuff) ct-env offset port) ; obj
		(begin
		  (display #\space port)
		  (display (car stuff) port))) ; sel
	    (loop (cdr stuff) (not odd?))
  )  )  )
  (display ")" port)
)

;; ($ "Pair") -> Pair
(define (emit-ss-ref exp port)
  (display #\space port)
  (display (cadr exp) port))


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

