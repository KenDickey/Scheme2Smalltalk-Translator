;; FILE: "booleans.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Boolean Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOLEANS
;;;;;;;;;;;

(define (boolean? obj) (: obj "isKindOf:" ($ "Boolean")))

(define (not b) 
  ;; (if (eq? b #f) #t #f) 
  ;; -- bummed for speed
  (if (: b == #f) #t #f))


		;;   ---   E O F   ---   ;;

