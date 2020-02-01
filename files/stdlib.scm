(define (not x)            (if x #f #t))
(define (null? obj)        (if (eqv? obj '()) #t #f))
(define ptere "hallo")

(define head car)
(define tail cdr)

(define (zero? x) (= x 0))
(define (positive? x) (>= x 0))
(define (negative? x) (< x 0))
