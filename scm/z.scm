
(define (char-newline? c)
  (or (: c "=" 10) (: c "=" 13)))

(define (count-newlines fname)
  (call-with-input-file fname
    (lambda (in)
      (let loop ( (ch (read-char in)) (char-count 0) (newline-count 0) )
	(cond
	 ((eof-object? ch)
	  (newline)
	  (display "chars: ")
	  (display char-count)
	  (display " newlines: ")
	  (display newline-count)
	  (newline))
	 ((char-newline? ch)
	  (loop (read-char in) char-count (+ 1 newline-count)))
	 (else
	  (loop (read-char in) (+ 1 char-count) newline-count))
	)
) ) ) )

(count-newlines "D:/SmallScript/ProtoScheme/SCM/bootstrap.scm")
