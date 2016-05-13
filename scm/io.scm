;; FILE: "io.scm"
;; IMPLEMENTS: Most of R^5RS Scheme Input/Output Procedures for ProtoScheme.
;; AUTHOR: Ken Dickey
;; DATE: 08 December 2001

;; COPYRIGHT (c) 2001, 2002 by Kenneth A Dickey
;; This software may be used for any purpose but
;; without warrenty or liability of any kind
;; provided this notice is included.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT and OUTPUT
;;;;;;;;;;;;;;;;;;;

;; @@ Squeak specific; full-pathname-for should not be visible
(define (full-pathname-for file-name-string)
  (: (: ($ "FileDirectory") "forFileName:" file-name-string) 
     "fullNameFor:" file-name-string)
)

(define (call-with-input-file  file-name-string proc)
  (let ( (inport (: ($ "(StandardFileStream new)") 
		    "open:" 
		    (full-pathname-for file-name-string)
		    "forWrite: false"))
       )
    (if (null? inport)
	(error "Could not open file" file-name-string)
	(: (lambda () (proc inport)) 
	   ensure: (lambda () (: inport close)))
) ) )


(define (call-with-output-file  file-name-string proc)
  (let ( (outport (: ($ "(StandardFileStream new)") 
		    "open:" 
		    (full-pathname-for file-name-string)
		    "forWrite: true"))
       )
    (if (null? outport)
	(error "Could not open file" file-name-string)
	(: (lambda () (proc outport)) 
	   ensure: (lambda () (: outport close)))
) ) )

;; FIXME: bogus?
(define (input-port? obj) 
  (or (: obj "isKindOf:" ($ "ReadStream"))
      (: obj "isKindOf:" ($ "ReadWriteStream"))
) )

(define (output-port? obj) 
  (or (: obj "isKindOf:" ($ "WriteStream"))
      (: obj "isKindOf:" ($ "ReadWriteStream"))
) )

;; NOT r5rs
(define (port? obj) (: obj "isKindOf:" ($ "Stream")))

;; NB: Dynamic variables live in a separate, per-thread namespace.
;; FIXME: per-thread initialization
(dynamic-define %*current-input-port*%  ($ nil)) ;; @@FIXME: Bogus NO DEFAULT@@
(dynamic-define %*current-output-port*% ($ "Transcript"))

(define (current-input-port)  
  (dynamic-ref-with-default %*current-input-port*% 
			    (lambda () ($ nil)))) ;; @@FIXME: Bogus@@


(define (current-output-port) 
  (dynamic-ref-with-default %*current-output-port*%
			    (lambda () ($ "Transcript"))))


(define (with-input-from-file file-name-string thunk)
  (let ( (inport (open-input-file file-name-string)) )
    (if (null? inport)
	(error "Could not open file" file-name-string)
	(dynamic-let ( (%*current-input-port*% inport) )
	   (: thunk 
	      ensure:
	      (lambda () (: inport close)))
) ) ) )


(define (with-output-to-file  file-name-string thunk)
  (let ( (outport (open-output-file file-name-string)) )
    (if (null? outport)
	(error "Could not open file" file-name-string)
	(dynamic-let ( (%*current-output-port*% outport) )
	   (: thunk 
	      ensure:
	      (lambda () (: outport close)))
) ) ) )


(define (open-input-file  file-name-string) ;;-> port
  (let ( (inport
	  (: ($ "(StandardFileStream new)") 
	     "open:" 
	     (full-pathname-for file-name-string)
	     "forWrite: false"))
       )
    (if (null? inport) 
	(error "Could not open file" file-name-string)
	inport)
) )


(define (open-output-file file-name-string) ;;-> port
  (let ( (outport
	  (: ($ "(StandardFileStream new)") 
	     "open:" 
	     (full-pathname-for file-name-string)
	     "forWrite: true"))
       )
    (if (null? outport) 
	(error "Could not open file" file-name-string)
	outport)
) )


(define (close-input-port  port) (: port close))
(define (close-output-port port) (: port close))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT
;;;;;;;;

;; @@ the-eof-object should not be visible
(define the-eof-object '"EOF") ;; not eq? to anything else

; (read . port) -- see "read.scm"

(define (read-char . optional-port)
  (let ( (inport (if (or (null? optional-port) 
			 (not (input-port? (car optional-port))))
		     (current-input-port)
		     (car optional-port)))
       )
    (if (: inport "atEnd")
	the-eof-object
	(: inport next))
) )

(define (peek-char . optional-port)
  (let ( (inport (if (or (null? optional-port) 
			 (not (input-port? (car optional-port))))
		     (current-input-port)
		     (car optional-port)))
       )
    (if (: inport "atEnd")
	the-eof-object
	(: inport "peek"))
) )

(define (eof-object? obj) (: obj == the-eof-object))

;; FIXME: bogus
(define (char-ready? port) #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OUTPUT
;;;;;;;;;

(define (write obj . optional-port)
  (let ( (outport 
	  (if (or (null? optional-port) 
		  (not (output-port? (car optional-port))))
	      (current-output-port)
	      (car optional-port)))
       )
    (: outport "nextPutAll:" (: obj "asSchemeObjString"))
) )

(define (display obj . optional-port)
  (let ( (outport 
	  (if (or (null? optional-port) 
		  (not (output-port? (car optional-port))))
	      (current-output-port)
	      (car optional-port)))
       )
    (: outport "nextPutAll:" (: obj "asSchemeString"))
) )

(define (newline . optional-port)
  (let ( (outport 
	  (if (or (null? optional-port) 
		  (not (output-port? (car optional-port))))
	      (current-output-port)
	      (car optional-port)))
       )
    (: outport "nextPut: Character cr")
) )


(define (write-char char . optional-port)
  (let ( (outport 
	  (if (or (null? optional-port) 
		  (not (output-port? (car optional-port))))
	      (current-output-port)
	      (car optional-port)))
       )
    (display char outport)
) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYSTEM INTERFACE
;;;;;;;;;;;;;;;;;;;


;; For the following, load "transcript.scm" after loading this file.
; (transcript-on file-name-string)
; (transcript-off)

;; SRFI-6 string ports

(define (open-output-string)
  (: ($ "WriteStream") with: ""))

(define (get-output-string output-string-port)
  (if (output-port? output-string-port)
      (: output-string-port contents)
      (error "Not an output-string-port" output-string-port)
) )

(define (open-input-string str)
  (: ($ "ReadStream") on: str from: 1 to: (: str size)))


		;;   ---   E O F   ---   ;;
