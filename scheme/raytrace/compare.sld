(define-library (raytrace compare)
  (export almost=)
  (import (scheme base)
          (raytrace constants))
  (begin
    (define (almost= a b)
      (or (< (abs (- a b))
             EPSILON)
          (and (= a +inf.0)
               (= b +inf.0))
          (and (= a -inf.0)
               (= b -inf.0))))

    (define (abs x) (if (< x 0) (- x) x))))
