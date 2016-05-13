;; xlate part 2

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

;; end xlate2.scm
