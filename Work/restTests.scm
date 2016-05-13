;; restTests.scm

(define (cons a b) (: ($ "Pair") car: a cdr: b))

(define (make-list . rest) rest)

(define foo (lambda all all))

(define (bar a b . others) (cons a (cons b others)))

(define baz (lambda (a b . others) (cons a (cons b others))))

(newline)
(display "Many ways to make a list")
(newline)
(display (make-list 1 2 3 4 5 6))
(newline)
(display (foo 1 2 3 4 5 6))
(newline)
(display (bar 1 2 3 4 5 6))
(newline)
(display (baz 1 2 3 4 5 6))
(newline)
(newline)
(display (make-list 1 2 3))
(newline)
(display (foo 1 2 3))
(newline)
(display (bar 1 2 3))
(newline)
(display (baz 1 2 3))
(newline)
(newline)
(display (make-list 1 2))
(newline)
(display (foo 1 2))
(newline)
(display (bar 1 2)) ;; bar and baz require at least 2 arguments
(newline)
(display (baz 1 2))
(newline)
(newline)
(display (make-list 1))
(newline)
(display (foo 1))
(newline)
(newline)
(display (make-list))
(newline)
(display (foo))
(newline)


