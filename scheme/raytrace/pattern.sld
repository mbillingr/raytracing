(define-library (raytrace pattern)
  (export make-pattern stripe-pattern)
  (import (scheme base) (scheme write)
          (raytrace tuple)
          (raytrace transformations)
          (raytrace matrix))
  (begin
    (define (make-pattern func)
      (define inv-transform (identity-transform))

      (define (dispatch m . args)
        (cond ((eq? m 'at) (func (m4* inv-transform (car args))))
              ((eq? m 'set-transform!)
               (set! inv-transform (m4-inverse (car args))))
              ((eq? m 'inv-transform) inv-transform)
              (else (error "unknown method (pattern m ...)" m))))
      dispatch)

    (define (stripe-pattern a b)
      (make-pattern
        (lambda (point)
          (if (= 0 (remainder
                     (floor (tuple-x point))
                     2))
              a
              b))))))
