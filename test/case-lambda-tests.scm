
(define plus
  (case-lambda 
   (()      0)
   ((x)     x)
   ((x y)   (+ x y))
   ((x y z) (+ x y z))
   (args (apply + args))
) )

(display (plus))         (newline)
(display (plus 1))       (newline)
(display (plus 1 2))     (newline)
(display (plus 1 2 3))   (newline)
(display (plus 1 2 3 4)) (newline)
(newline)
(define foo 
  (case-lambda
   (() 1)
   ((x) (+ 1 x))
   ((x . y) (cons x y))))
(newline) (display (foo))
(newline) (display (foo 1))
(newline) (display (foo 1 2))
(newline) (display (foo 1 2 3))
(newline) (display (foo 1 2 3 4))

(define bar
  (case-lambda
   (() 1)
   ((x) (+ 1 x))
   ((x y) (list x y))))

(newline) (display (bar))
(newline) (display (bar 1))
(newline) (display (bar 'a 'b))
(bar 'a 'b 'c) ;; error

