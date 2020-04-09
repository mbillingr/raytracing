(define-library (raytrace pattern)
  (export stripe-pattern)
  (import (scheme base) (scheme write)
          (raytrace tuple)
          (raytrace transformations)
          (raytrace matrix))
  (begin
    (define (stripe-pattern a b)
      (define transform (identity-transform))
      (define inv-transform (identity-transform))

      (define (func point)
        (if (= 0 (remainder
                   (floor
                     (tuple-x
                       (m4* inv-transform
                            point))) 
                   2))
            a
            b))

      (define (dispatch m . args)
        (cond ((eq? m 'at) (func (car args)))
              ((eq? m 'a) a)
              ((eq? m 'b) b)
              ((eq? m 'set-transform!)
               (set! transform (car args))
               (set! inv-transform (m4-inverse (car args))))
              (else (error "unknown method (pattern m ...)" m))))
      dispatch)))
