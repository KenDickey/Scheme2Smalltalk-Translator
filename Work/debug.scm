(define (read-echo file-name-string)
  (call-with-input-file file-name-string
    (lambda (in)
      (let loop ( (form (read in)) )
	(if (eof-object? form)
	    'done
	    (begin
	      (newline)
	      (display form)
	      (loop (read in))))))))

(define (xlate-from-file file-name-string . outport)
  (let ( (out (if (null? outport) (current-output-port) (car outport))) )
    (call-with-input-file file-name-string
      (lambda (in)
        (newline out)
        (display "include path: ProtoScheme.sts." out)
        (newline out)
        (display "[" out)
        (let loop ( (form (read in)) )
          (if (eof-object? form)
              'done
              (begin
              (newline out)
              (display "/* " out)
              (display form out)
              (display " */" out)
              (translate form out)
              (loop (read in)))))
        (display "]" out)
        (newline out)))))


(define (xlate-file infile outfile)
  (call-with-output-file outfile
      (lambda (out)
        (xlate-from-file infile out))))



