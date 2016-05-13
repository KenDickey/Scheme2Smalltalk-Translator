;; "dynamicsTest.scm" 

(dynamic-define radix 10)

(define (f x) (+ x (dynamic-ref radix)))

(define (test-it expected)
  (display "expect ")
  (display (+ 100 expected))
  (display " got: ")
  (display (f 100)) 
  (newline))

(test-it 10)

(dynamic-let ( (radix 2) )
   (test-it 2)
   (dynamic-set! radix 3)
   (test-it 3))

(test-it 10)

; (define (f x) (number->string x (dynamic-ref radix)))

; (list (f 5) (f 15))
; ;;->("5" "15")

; (dynamic-let ((radix 2))
;   (list (f 5) (f 15)))
; ;;-> ("101" "1111")
