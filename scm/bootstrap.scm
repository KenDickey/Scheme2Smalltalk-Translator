;; Gambit assumed
;;(load "/home/kend/gambc.scm")

;; ,open signals
;; (load "D:/SmallScript/ProtoScheme/SCM/bootstrap.scm") ;;this file

;; @@@ Scheme48's char->integer is non-standard @@@
;; ,open ascii
;(define char->integer char->ascii)


(define gensym
  (let ( (counter 0 ) )
    (lambda (string-or-symbol)
      (set! counter (+ counter 1))
      (string->symbol
       (string-append 
	(if (symbol? string-or-symbol)
	    (symbol->string string-or-symbol)
	    string-or-symbol)
	(number->string counter))))))

(define base-dir "/usr/local/src/Squeak/ProtoScheme/")
(define sappend string-append)
(define scm-dir (sappend base-dir "scm/"))
(define sts-dir (sappend base-dir "sts/"))
(define out-dir (sappend base-dir "out/"))

;; (require 'pretty-print)
(load (sappend scm-dir "pretty-print.scm"))


(define (reload)
;;  (load (sappend scm-dir "xlate.scm")))
  (load (sappend scm-dir "xlate1.scm"))
  (load (sappend scm-dir "xlate2.scm"))
  (load (sappend scm-dir "xlate3.scm")))


(define (xlate-from-to file-stem)
 (xlate-file (sappend scm-dir file-stem ".scm")
             (sappend out-dir file-stem ".sts")
	     file-stem))

(define (xlate-library-from-to file-stem)
 (xlate-library-file (sappend scm-dir file-stem ".scm")
		     (sappend out-dir file-stem ".sts")
		     file-stem))


(define library-file-names
  '("numbers"
    "booleans"
    "lists"
    "symbols"
    "characters"
    "strings"
    "vectors"
    "control"
    "io"
    "ratize"
    "read"
    "string2number"
    "transcript"
    "format"
    "pretty-print"
)  )

(define user-file-names
  '(
    "eval"
    "trace"
;;    "xlate"
    "xlate1"
    "xlate2"
    "xlate3"
;;    "rwtest"
;;    "test"    
    "test1"    
    "test2"    
    "test3"    
    "test4"    
    "test5"    
    "test6"    
    "test7"
    "test8"
   )
)

(define (make-lib)
  (for-each (lambda (base-fname)
	      (newline)
	      (display "Translating: ")
	      (display base-fname)
	      (xlate-library-from-to base-fname))
	    library-file-names)
  (newline)
  (display "*=> Completed translating library files")
  (newline)
)

(define (make-tests)
  (for-each (lambda (base-fname)
	      (newline)
	      (display "Translating: ")
	      (display base-fname)
	      (xlate-from-to base-fname))
	    user-file-names)
  (newline)
  (display "*=> Completed translating user&test files")
  (newline) (newline)
)


(define (make-all)
  (make-lib)
  (make-tests))

;; lex ok?
(define (read-echo file-name-string)
  (call-with-input-file file-name-string
    (lambda (in)
      (let loop ( (form (read in)) (count 1))
        (if (eof-object? form)
            'done
            (begin
            (newline)
            ;;(display count) (display "  ")
            (write form)
            (loop (read in) (+ count 1))))))))


;; load initially

   (reload)

;;(define sharp-quote-test #'aSymbol)
;;(define quote-test 'aSymbol)

                ;;  --- E O F --   ;;
