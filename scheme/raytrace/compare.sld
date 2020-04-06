(define-library (raytrace compare)
  (export almost=)
  (import (scheme base)
          (raytrace constants))
  (begin
    (define (almost= a b)
      (< (abs (- a b))
         EPSILON))

    (define (abs x) (if (< x 0) (- x) x))))
