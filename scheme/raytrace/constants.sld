(define-library (raytrace constants)
  (export PI SQRT2)
  (import (scheme base)
          (scheme inexact))
  (begin
    (define PI 3.14159265358979323846264338327950288419716939937510582)
    (define SQRT2 (sqrt 2))))
