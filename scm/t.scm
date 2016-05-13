
($ "include path: format.sts")
($ "include path: format.sts")
($ "include path: pretty-print.sts")

($ "/* @@ DEBUG -- lookup on 'number->string fails if removed @@ */")
(define (number->string z radix)
  (case radix
    ((2 8 10 16) (: z "asStringRadix:" radix))
    (else (: z "asStringRadix:" 10))
) )

(newline)
(display (number->string 32 2))
(newline)
(display (number->string 32 8))
(newline)
(display (number->string 32 10))
(newline)
(display (number->string 32 16))

(format #t "~%~a ~s ~a ~s ~%~t32 is  #b~b #o~o #d~d #x~x ~%" 
	'this 'is "a" "test" 32 32 32 32 32)


(define test-exp
 '(define (read-echo file-name-string)
  (call-with-input-file file-name-string
    (lambda (in)
      (let loop ( (form (read in)) (count 1))
        (if (eof-object? form)
            'done
            (begin
            (newline)
            (display count) (display "  ")
            (write form)
            (loop (read in) (+ count 1)))))))))

(format #t "~%~s~%" test-exp)
(format #t "~%~a~%" test-exp)
(format #t "~%~g~%" test-exp)

(define (read-echo file-name-string)
  (call-with-input-file file-name-string
    (lambda (in)
      (let loop ( (form (read in)) (count 1))
        (if (eof-object? form)
            'done
            (begin
            (newline)
            (display count) (display "  ")
            (pretty-print form)
            (loop (read in) (+ count 1))))))))

(define (test-read)
   (read-echo "d:/SmallScript/ProtoScheme/SCM/read.scm"))

(test-read)

    