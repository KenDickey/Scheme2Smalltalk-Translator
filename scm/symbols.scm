;; FILE: "symbols.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Symbol Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;; NB: assumes intern case is lower-case.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOLS
;;;;;;;;;;

(define (symbol? obj) (: obj "isKindOf:" ($ "Symbol")))

(define (symbol->string symbol) (: symbol "asSchemeString"))

(define (string->symbol string) 
  (: (: string "fromSchemeIdentifier") "asSymbol"))


;; NOT r5rs
(define gensym
  (let ( (counter 0 ) )
    (lambda optional-str-or-sym
      (let ( (string-or-symbol (if (null? optional-str-or-sym)
				   "g"
				   (car optional-str-or-sym)))
	   )
	(set! counter (+ counter 1)) ;; (: counter ":+=" 1)
	(string->symbol
	 (string-append 
	  (if (symbol? string-or-symbol)
	      (symbol->string string-or-symbol)
	      string-or-symbol)
	  (number->string counter)))
) ) ) )


		;;   ---   E O F   ---   ;;
