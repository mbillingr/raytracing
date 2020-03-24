(define-library (raytrace compare)
  (export almost=)
  (import (scheme base))
  (begin
    (define EPSILON 0.00001)

    (define (almost= a b)
      (< (abs (- a b))
         EPSILON))

    (define (abs x) (if (< x 0) (- x) x))))
