;; FILE: "eval.scm"
;; IMPLEMENTS: EVAL for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 19 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;; REQUIRES: TRANSLATE -- see file "xlate.scm"

;; FIXME -- namespaces
;; FIXME -- born turtle slow
(define (eval exp env-spec)
  (let* ( (string-port (open-output-string)) 
	  (scheme-exp (if (string? exp) exp (: exp "asSchemeObjString")))
	  (st-exp  (: (scheme2smalltalk scheme-exp string-port) contents))
	)
    ( (: ($ "Utilities") evaluate: st-exp in: ($ nil) to: ($ nil))
      env-spec )
) )

;; FIXME: bogus
(define (scheme-report-environment version) (: ($ "SmallScheme") "newEnv"))
(define (null-environment version)          (: ($ "SmallScheme") "newEnv"))
(define (interaction-environment) (: ($ "schemeEnv")))

;; FIXME
(define (load file-name-string)
  (call-with-input-file file-name-string
    (lambda (in)
      (let loop ( (form (read in)) )
        (if (eof-object? form)
            file-name-string
	    (begin 
	      (eval form (interaction-environment))
	      (loop (read in))
) ) ) ) ) )


		;;   ---   E O F   ---   ;;
