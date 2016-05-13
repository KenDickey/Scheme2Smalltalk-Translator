;; FILE: "read.scm"
;; IMPLEMENTS: A simple R^5RS Scheme reader written in core scheme.
;; AUTHOR: Ken Dickey
;; DATE: 11 December 2001

;; COPYRIGHT (c) Kenneth A Dickey 2001, 2002
;; You may use this file for any purpose
;; but without warrenty or liability of any kind
;; provided this notice remains intact.

;; NOTES:
;;  Readers are hairy. This code is meant to be comprehensible.
;;  How simple can I make it? 

;;  Warning.  This is expository code (read slow).  Grep for
;;  "speed hack" below.

;; Characters reserved for future language use are: [ ] { } |

;; Case is insignificant in symbols.

;; NB: Assumes ASCII !!  (Dumb, but cheap)

;; Constant strings are quoted.  [Some compilers are 
;; smart and make them immutable.  This conses enough 
;; already!].

;; Tested with Scheme48, Gambit, MIT Scheme, ProtoScheme.

;; ADDITIONAL SYNTAX:
;;  #'aSymbol -> (string->symbol "aSymbol")

;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv;;
;; @@@ Scheme48's char->integer is non-standard @@@;;
;; ,open ascii
;(define char->integer char->ascii)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ -- A simple Scheme reader.
;;;;;;;

(define (read . input-port)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character Classes
;;;;;;;;;;;;;;;;;;;;

;; Something to initialize tables with.

(define whitespace '( #\space #\newline ))

(define delimiters (append (list #\( #\) #\" #\; ) whitespace))

(define special-initial-chars '"!!$%&/:*<=>?^_~" ) ;; #\! is doubled because of Smallscript text encoding
(define special-initials (string->list special-initial-chars))

(define special-subsequent-chars '"+-.@" )

(define pecular-identifier '( + - ... ))

(define pecular-initials '( #\+ #\- #\. ))

(define syntactic-keywords '( else => define unquote unquote-splicing ))

(define expression-keywords
  '( quote lambda if set! begin cond and or case 
	   let let* letrec do delay quasiquote ))

;; NB: Implementation Dependent
(define character-names-alist `(  (space    . ,(integer->char 32))
				  (tab      . ,(integer->char  9))
				  (return   . ,(integer->char 13))
				  (newline  . ,(integer->char 10))
				  (formfeed . ,(integer->char 12))
                               )
)

(define token-classes 
  '( identifier boolean number character string 
     lparen rparen sharp-lparen quote backquote 
;;     (       )        #(        '      `
     comma comma-splice period ))
;;     ,        ,@         .

(define abbreviation-starts '( #\, #\' #\` ))

(define radix-chars     '"bodx" )
(define exactness-chars '"ie")

(define letter-chars 
  '"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define digit-chars '"0123456789")

(define num-2-chars '"01")
(define num-8-chars '"01234567")
(define num-10-chars digit-chars)
(define num-16-chars '"01234567689abcdefABCDEF")

(define exponent-marker-chars '"esfdl")

(define sign-chars '"+-")

(define legal-number-chars-list
  (append (string->list sign-chars)
	  (string->list num-16-chars)
	  (string->list exponent-marker-chars)
	  (string->list radix-chars)
	  (string->list exactness-chars)
	  (list #\. #\# #\@ #\/ )
) )

(define subsequents 
  (append (string->list letter-chars)
	  (string->list digit-chars)
	  (string->list special-initial-chars)
	  (string->list special-subsequent-chars)
) ) 


(define rparen-marker ")") ;; Something to be eq? to.
(define period-marker ".") ;;  ditto

;; Intern Case

(define string->caseified-symbol
  (let ( (squash-case 
	  (if (eq? #\a (string-ref (symbol->string 'A) 0))
	      char-downcase
	      char-upcase))
       )
    (lambda (str)
      (string->symbol (list->string (map squash-case (string->list str)))))
) )



(define (read1 port)
  (let loop ( (next (peek-char port)) )
    (cond
     ((eof-object?      next)     
      next
     )
     ((char-whitespace? next) 
      (read-char port)
      (loop (peek-char port))
     )
     ((comment-char?    next)
      (consume-comment port)
      (loop (peek-char port))
     )
     ((sharp-char?      next) (read-sharp-thingie port))
     ((char-numeric?    next) (read-number        port))
     ((char-alphabetic? next) (read-identifier    port))
     ((string-char?     next) (read-string        port))
     ((abbrev-char?     next) (read-abbreviation  port))
     ((lparen-char?     next) (read-list          port))
     ((special-initial? next) (read-identifier    port))
     ((pecular-initial? next) (read-pecular       port))
     ((rparen-char?     next)
      (consume-char port)
      rparen-marker ;; end of list
     )
     (else (error "read: unacceptable character with code:"
		  (char->integer next)))
) ) )

(define (comment-char? c) (eq? c #\; ))
(define (sharp-char?   c) (eq? c #\# ))
(define (string-char?  c) (eq? c #\" ))
(define (lparen-char?  c) (eq? c #\( ))
(define (rparen-char?  c) (eq? c #\) ))

;; NOTE: The typical speed hack is to make a vector
;; of the ASCII characters and use bit fields to
;; determine which character-class a character 
;; belongs to.  I.e. 
;; (define (get-char-class-encoding char)
;;     (vector-ref char-class-vec (char->integer char)))
;; Then:
;; (define (special-initial? c)
;;    (not (zero? (bit-and (get-char-class-encoding c)
;;                          special-initial-bit))))

(define (special-initial? c)
  (memv c special-initials)) ;; FIXME: bum for speed

(define (pecular-initial? c)
  (memv c pecular-initials)) ;; FIXME: bum for speed

(define (identifier-subsequent-char? c)
  (memv c subsequents)) ;; FIXME: bum for speed

(define (abbrev-char? c)
  (memv c abbreviation-starts)) ;; FIXME: bum for speed

(define (legal-number-char? c)  ;; FIXME: bum for speed
  (memv c legal-number-chars-list))
      
;; To note that the value is ignored.
(define (consume-char port) 
  (if (eof-object? (read-char port))
      (error "Unexpected End Of File"))
) ;; unspecific


(define (consume-comment port) 
  ;; ASSERT: peek-char is #\;
  (consume-char port)
  (let loop ( (next (peek-char port)) )
    (if (not (or (eof-object? next) 
		 (char-newline? next)))
	(begin
	  (consume-char port)
	  (loop (peek-char port)))
	'done) ;; don't consume newline
) )


;; #t #f #( #\c #\<charname> #e #i #o #b #d #x
;; #'aSymbol
(define (read-sharp-thingie port)
  ;; ASSERT: peek-char is #\#
  (consume-char port)
  (let ( (next (peek-char port)) )
    (case next
      (( #\t #\f #\T #\F ) (make-boolean port))
      (( #\e #\i #\o #\b #\d #\x #\E #\I #\O #\B #\D #\X )
       (read-number-loop '(#\#) port))
      (( #\\ ) (read-character port))
      (( #\( ) (read-vector    port))
      (( #\' ) (read-sharp-sym port))
      (else (error "Unknown sharp syntax" next)))
) )

(define (make-boolean port)
  ;; ASSERT peek-char is #\t or #\f
  (case (read-char port)
    ((#\t #\T) #t)
    ((#\f #\F) #f)
    (else (error "reading boolean: expected #t or #f here"))
) )

(define (read-sharp-sym port)
  ;; ASSERT: peek-char is #\', #\# already read
  (consume-char port) ; eat #\'
 `(string->symbol ,(read-identifier-string port))
)

; (define (read-symbol-retaining-case port)
;   ;; ASSERT: peek-char is #\', #\# already read
;   (consume-char port) ; eat #\'
;   (let ( (lead-char (peek-char port)) )
;     (if (or (char-alphabetic? lead-char)
; 	    (special-initial? lead-char))
; 	(string->symbol (read-identifier-string port))
; 	(error "expected a symbol" lead-char)
; ) ) )

(define (read-character port)
  ;; ASSERT: peek-char is #\\, #\# already read
  (consume-char port) ; eat #\\
  (let* ( (next (peek-char port)) )
    (cond
     ((eof-object? next)
      (error "EOF parsing character while reading file")
     )
     ((char-alphabetic? next)
      (let ( (char-name-string (read-identifier-string port)) )
	(cond ((= 1 (string-length char-name-string))
	       (string-ref char-name-string 0)
	      )
	      ((assq (string->caseified-symbol char-name-string) 
		     character-names-alist)
	       => (lambda (bucket) (cdr bucket))
	      )
	      (else (error "read: invalid character name" char-name-string))
	)
     ))
     (else (read-char port)) ;; just return it
) ) )


;; NB: NOT read-identifier; returns a string, not a symbol
(define (read-identifier-string port)
  ;; ASSERT: 1st char is legal identifier char
  (let loop ( (chars (list (read-char port))) )
    (let ( (next (peek-char port)) )
      (cond
       ((eof-object? next)
	(list->string (reverse chars))
       )
       ((identifier-subsequent-char? next)
	(loop (cons (read-char port) chars))
       )
       (else (list->string (reverse chars)))
      )
) ) )

(define (read-number port)
  ;; ASSERT: peek-char is number start syntax
  (read-number-loop (list (read-char port)) port))


;; used multiple places
(define (read-number-loop chars port)
  (let ( (next (peek-char port)) )
    (cond
     ((eof-object? next)
      (string->number (list->string (reverse chars)))
     )
     ((legal-number-char? next) ;; FIXME: number syntax recognizer
      (read-number-loop (cons (read-char port) chars) port)
     )
     (else
      (string->number (list->string (reverse chars))))
    )
) )


(define (read-identifier port)
  ;; ASSERT: peek-char is start of identifier
  (string->caseified-symbol (read-identifier-string port)))


(define (read-abbreviation port)
  ;; ASSERT: peek-char is 1st char of #\` #\' #\, or ",@"
  (case (read-char port)
    ((#\`) (list 'quasiquote (read1 port))
    )
    ((#\') (list 'quote	     (read1 port))
    )
    ((#\,)
     (if (eq? #\@ (peek-char port))
	 (begin
	   (consume-char port)
	   (list 'unquote-splicing (read1 port)))
	 (list 'unquote (read1 port))
    ))
    (else (error "Assert failed in reading abbreviation"))
) )


(define (read-string port)
  ;; ASSERT: peek-char is #\"
  (consume-char port)
  (let loop ( (chars '()) )
    (let ( (next (peek-char port)) )
      (cond
       ((eof-object? next)
	(error "read: End Of File reading string"))
       ((eq? next #\")
	(consume-char port)
	(list->string (reverse chars))
       )
       ((eq? next #\\)
	(consume-char port)
	(let ( (following (peek-char port)) )
	  (cond
	   ((eof-object? following)
	    (error "EOF in read-string")
	   )
	   ((memv following '( #\" #\\ ))
	    (loop (cons (read-char port) chars))
	   )
	   (else (error "Illegal char following #\\" following))
       )) )
       (else (loop (cons (read-char port) chars)))
) ) ) )


;; Pecular identifiers are '+ '- '...
(define (read-pecular port) 
  ;; ASSERT: peek-char is one of #\+ #\- #\.
  (let* ( (char (read-char port)) (next (peek-char port)) )
    (cond
     ((memv char '( #\+ #\- ))
      (if (legal-number-char? next)
	  (read-number-loop (list char) port)
	  (if (eq? char #\+) '+ '-)
     ))
     ((eq? #\. char)
      (if (eq? #\. next)
	  (begin
	    (consume-char port)
	    (if (eq? #\. (peek-char port))
		(begin 
		  (consume-char port)
		  '...)
		(error "Illegal period literal")
	  ) )
	  period-marker
     ))
     (else (error "Assert failed in reading pecular identifier" char))
) ) )

(define (period-marker? thing)
  (eq? period-marker thing))

(define (rparen-marker? thing)
  (eq? rparen-marker thing))

(define (read-list port) ;; returns a list of tokens
  ;; ASSERT peek-char is #\(
  (consume-char port)
  (let loop ( (result '()) )
    (let ( (thing (read1 port)) )
      (cond
       ((eof-object? thing)
	(error "unexpected end of file reading list"))
       ((rparen-marker? thing) 
	(reverse result)
       )
       ((period-marker? thing) ;; dotted tail
	(let ( (next (read1 port)) )
	  (if (memv next `(,period-marker ,rparen-marker))
	      (error "read: ill-formed list")
	      (let ( (final (read1 port)) )
		(if (rparen-marker? final)
		    (set-last-pair (reverse result) next)
		    (error "read: ill-formed dotted list tail")
       )) )  )  )
       (else (loop (cons thing result)))
) ) ) )


(define (read-vector port)
  ;; ASSERT: peek-char is #\( , #\# preceeded it
  (let ( (list (read-list port)) )
    (if (list? list)
	(list->vector list)
	(error "read: ill-formed vector"))
) )

;;(define (read . input-port) ;;@@@
 (read1 (if (null? input-port) 
	     (current-input-port) 
 	     (car input-port)))
) ; end (read)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG
;;;;;;;;

; (define (debug-read file-name-string)
;   (call-with-input-file file-name-string
;     (lambda (in)
;       (newline)
;       (display "** reading =====> ")
;       (display file-name-string)
;       (display " **")
;       (newline)
;       (let loop ( (form (read1 in)) )
; 	(if (eof-object? form)
; 	    (begin
; 	      (newline)
; 	      (display "** completed reading file ===== **")
; 	      (display form)
; 	      (newline))
; 	    (begin
; 	      (newline)
; 	      (write form) ;; pretty-print is prettier.
; 	      (loop (read1 in))))))))


              ;;   --- E O F ---   ;;
