;; Primitive trace facility

(define *traced* '())

(define (trace fname)
  (let ( (fval (: ($ "self") "globalRef:" fname)) )
    (cond
     ((assq fname *traced*)
      => (lambda (bucket) (set-cdr! bucket fval)))
     (else (set! *traced* (cons (cons fname fval) *traced*)))
     )
    (let ( (traced-fun
	    (lambda args
	      (newline)
	      (display "**entering** ")
	      (write fname)
	      (display " < ")
	      (write args)
	      ;;	  (newline)
	      (let ( (result (apply fval args)) )
		(newline)
		(display "***leaving** ")
		(write fname)
		(display " > ")
		(write result)
		(newline)
		result)))
	   )
      (: ($ self) "defineOrSet:" ($ fname) "as:" traced-fun)
      (map car *traced*)
) ) )

(define (untrace fname)
  (cond
   ((assq fname *traced*)
    => (lambda (bucket) (set! ($ fname) (cdr bucket))))
   )
  (map car *traced*)
)

(define (untrace-all) (map (lambda (fname) (untrace fname)) *traced*))
