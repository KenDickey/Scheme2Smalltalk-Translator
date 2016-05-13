;;;; "ratize.scm" Find simplest number ratios
;; FROM SLIB

(define (find-ratio-between x y)
  (define (sr x y)
    (let ((fx (inexact->exact (floor x))) (fy (inexact->exact (floor y))))
      (cond ((>= fx x) (list fx 1))
	    ((= fx fy) (let ((rat (sr (/ (- y fy)) (/ (- x fx)))))
			 (list (+ (cadr rat) (* fx (car rat))) (car rat))))
	    (else (list (+ 1 fx) 1)))))
  (cond ((< y x) (find-ratio-between y x))
	((>= x y) (list x 1))
	((positive? x) (sr x y))
	((negative? y) (let ((rat (sr (- y) (- x))))
			 (list (- (car rat)) (cadr rat))))
	(else '(0 1))))

(define (find-ratio x e) (find-ratio-between (- x e) (+ x e)))

(define (rationalize x e) (apply / (find-ratio x e)))
