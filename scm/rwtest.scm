; (define (read-display file-name-string)
;   (with-input-from-file file-name-string
;     (lambda () 
;       (display "Reading from ")
;       (display file-name-string)
;       (newline)
;       (let loop ( (form (read)) )
; 	(if (eof-object? form)
; 	    'done
; 	    (begin
; 	      (newline)
; 	      (display form)
; 	      (loop (read))))
; ) ) ) )

; @@ DEBUG
; (trace 'read1 read1)
; (trace 'read-sharp-thingie read-sharp-thingie)
; (trace 'read-sharp-sym read-sharp-sym)
; (trace 'read-character read-character)
; (trace 'read-identifier-string read-identifier-string)
; (trace 'read-number read-number)
; (trace 'read-identifier read-identifier)
; (trace 'read-abbreviation read-abbreviation)
; (trace 'read-string read-string)
; (trace 'read-pecular read-pecular)
; (trace 'read-list read-list)
; (trace 'read-vector read-vector)
; (trace 'read read)


(define (read-display file-name-string)
  (let ( (in (open-input-file file-name-string)) )
    (display "Reading from ")
    (display file-name-string)
    (newline)
    (let loop ( (form (read in)) )
      (if (eof-object? form)
	  (close-input-port in)
	  (begin
	    (newline)
	    (display form)
	    (loop (read in)))))
) )

(define (read-write file-name-string)
  (with-input-from-file file-name-string
    (lambda () 
      (write "Reading from ")
      (write file-name-string)
      (newline)
      (let loop ( (form (read)) )
	(if (eof-object? form)
	    'done
	    (begin
	      (newline)
	      (write form)
	      (loop (read))))
) ) ) )


(define (read-pp file-name-string)
  (with-input-from-file file-name-string
    (lambda () 
      (write "Reading from ")
      (write file-name-string)
      (newline)
      (let loop ( (form (read)) )
	(if (eof-object? form)
	    'done
	    (begin
	      (newline)
	      (pretty-print form)
	      (loop (read))))
) ) ) )
	      
