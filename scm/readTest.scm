;; (require 'pretty-print)

(define (debug-read file-name-string)
  (call-with-input-file file-name-string
    (lambda (in)
      (newline)
      (display "** reading =====> ")
      (display file-name-string)
      (display " **")
      (newline)
      (let loop ( (form (read in)) )
	(if (eof-object? form)
	    (begin
	      (newline)
	      (display "** completed reading file ===== **"))
	    (begin
	      (newline)
	      (pretty-print form)
	      (loop (read in))))))))

(debug-read "/usr/local/src/Squeak/ProtoScheme/scm/bootstrap.scm")
