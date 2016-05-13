;; xlateTests.scm

;; (load "D:/SmallScript/ProtoScheme/debug.scm")
;; (load "D:/SmallScript/ProtoScheme/xlate.scm") 
;; (test-xlate "D:/SmallScript/ProtoScheme/xlateTests.scm") 
;; (xlate-file "D:/SmallScript/ProtoScheme/xlateTests.scm" "D:/SmallScript/ProtoScheme/out.sts")

;; OR

;;  (load "D:/SmallScript/ProtoScheme/runtest.scm")


(define a 3)

(define b (+ a 5))

(define (f x) 3)

(define (g x) (+ x 1))

(define (h x y) (* x (+ y b)))

(display "(h 2 3) is ")
(display (h 2 3))
(newline)

(let ( (a 1) (b 2) )
  (display "(let ((a 1)(b 2))(+ a b)) is ")
  (display (+ a b)))

(newline)

(let loop ( (n 5) (a 1) )
  (display "n a = ")
  (display n)
  (display " ")
  (display a)
  (newline)
  (if (< n 2)
      (display a)
      (loop (- n 1) (* n a))))

(newline)

(define (fact n)
  (let loop ((n n)(a 1))
    (if (< n 2) a (loop (- n 1) (* n a)))))

(display "fact 50 is ")
(display (fact 50))
(newline)

(display "a is ")
(display a)
(newline)
(display "b is ")
(display b)
(newline)
(let* ( (a b) (b (+ a 1)) )
  (display "(let* ((a b)(b (+ a 1)))(+ a b)) is ")
  (display (+ a b)))

(newline)
(letrec ( (even (lambda (n) (if (= n 0) #t (odd  (- n 1)))))
          (odd  (lambda (n) (if (= n 0) #f (even (- n 1)))))
        )
  (display "(even 7) is ")
  (display (even 7))
  (newline)
  (display "(odd 17) is ")
  (display (odd 17)))

(newline)

(cond
 ((eqv? #t #f) (error "true is false?!?"))
 ((= 1 2) (error "1 is 2 ?!?"))
 (else (display "cond1-ok")))

(newline)

(cond
 ((eqv? #t #f) (error "true is false?!?"))
 ((= 1 2) (error "1 is 2 ?!?"))
 ((= (fact 5) 120) (display "cond2-ok"))
 (else (error "cond fell off end")))

(newline)

(display "b is ")
(display b)
(newline)
(display "(set! b 'sym)")
(newline)
(set! b 'sym)
(display "b is ")
(write b)
(newline)
(display "a should be 3, is: ")
(let ( (a 5) )
  (if (> a 1)
      (set! a 3))
  (display a))

(newline)

;;;; The following works:
;;(display "About to call ERROR")
;;(error "Test error!" 'works)

(newline)
(: (current-output-port) << "Hello from" << " Smallscript!")
(newline)
(display (: "Hello again" "asString")) ;; Note MIXED CASE => use string.
(newline)

(define (cons a b) (: ($ "Pair") car: a cdr: b))
(define (car c) (: c car))
(define (cdr c) (: c cdr))

(display "(list #\\c \"str\" () #t 'sym 34 (a b (c) d)) is ")
(newline)
(display "      ")
(display (list #\c "str" '() #t 'sym 34 '(a b (c) d)))
(newline)

;; Boy is this convoluted, er, clever! 8^(
(define count 0)
(define p
  (delay
   (begin
     (set! count (+ count 1))
     (if (> count x)
	 count
	 (force p)))))
(define x 5)
;;
(newline)
(display "should show 6 and 6")
(newline) (display p) ;; a promise
(newline) (display (force p)) ;; -> 6
(newline) (display p) ;; a promise still
(begin
  (newline)
  (set! x 10)
  (display (force p))) ;; -> 6

(newline)
(display "expected: neither, got: ")
(display
 (let ( (x 0) )
   (cond ((< x 0) 'negative)
	 ((> x 0) 'positive)
	 (else 'neither))))

(newline)
(display "expected: composit, got: ")
(display
 (case (* 2 3)
   ((2 3 5 7) 'prime)
   ((1 4 6 8 9) 'composit)))

(newline)
(display  "expected: consonant, got: ")
(display
 (case (car '(c d))
   ((a e i o u) 'vowel)
   ((w y) 'semivowel)
   (else 'consonant)))

(newline)
(display  "expected: 2, got: ")
(display
 (cond ((assv 'b '((a 1) (b 2))) => cadr)
       (else #f)))

(newline)
(display  "expected: 25, got: ")
(display
 (let ( (x '(1 3 5 7 9)) )
   (do ( (x x (cdr x))
	 (sum 0 (+ sum (car x)))
       )
       ((null? x) sum)) ))

(newline)
(display  "expected: #(0 1 2 3 4), got: ")
(display
 (do ( (vec (make-vector 5))
       (idx 0 (+ idx 1))
     )
     ((= idx 5) vec)
   (vector-set! vec idx idx)))
    

#t

              ;;   ---   E O F   ---   ;;
