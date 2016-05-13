;; FILE: "ProtoLib.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001
;;       24 December 2001 -- Broke up into separate files.

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;; NOTE:
;;  (: obj selector obj2 sel2 ... objN) -> Smallscript message send.
;;  ($ "Pair") -> Smallscript value.

;; Don't forget to use strings for selectors/var-names where case
;; is important.
;;
;; E.g.
;;  (define (cons a b) (: ($ "Pair") car: a cdr: b))
;; or
;;  (define (cons a b) (: ($ "Pair") "car:" a "cdr:" b))

(let ( (files-list
	'("numbers.scm"
	  "booleans.scm"
	  "lists.scm"
	  "symbols.scm"
	  "characters.scm"
	  "strings.scm"
	  "vectors.scm"
	  "control.scm"
	  "io.scm"
	  "eval.scm"
	  "ratize.scm"
	  "read.scm"
	  "string2number.scm"
	  "transcript.scm" 
     )

  (map (lambda (file-name)
	 (load (string-append "d:/SmallScript/ProtoScheme/SCM/" file-name)))
       files-list)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOT in R5RS
;;;;;;;;;;;;;;

;;;
;; Code for the following is above in "obvious" places
;
; (reduce op seed list)
; (uncommunitive-reduce op seed things)
; (nary-and op seed list)
; (any pred list)
; (fctorial n)
; (degrees->radians d)
; (radians->degrees r)
; (append! list1 list2)
; (last-pair list) 
; (set-last-pair list )
; (set-last-pair! list) 
; (rassq obj alist)
; (port? obj)
; (gensym . sym-or-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;;   ---   E O F   ---   ;;
