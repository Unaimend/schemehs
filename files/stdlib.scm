(define (not x)            (if x #f #t))
(define (null? obj)        (if (eqv? obj '()) #t #f))

(define head car)
(define tail cdr)

(define (zero? x) (= x 0))
(define (positive? x) (>= x 0))
(define (negative? x) (< x 0))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (head lst)) (tail lst))))

(define (map proc lis)
   (if (null? lis)
          '()
          (cons (proc (car lis)) (map proc (cdr lis)))))



