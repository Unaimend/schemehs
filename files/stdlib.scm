(define (not x)            (if x #f #t))
(define (null? obj)        (if (eqv? obj '()) #t #f))
(define ptere "hallo")

(define head car)
(define tail cdr)
