
(define stuff (list #\c "str" 'sym '#(1 2) '() #t #f 343 '(a b c)))

(display "display:")
(newline)
(for-each (lambda (thing) (display thing) (newline)) stuff)
(newline)
(display "write:")
(newline)
(for-each (lambda (thing) (write thing) (newline)) stuff)

