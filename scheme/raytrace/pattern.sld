(define-library (raytrace pattern)
  (export make-pattern
          checkers-pattern gradient-pattern
          ring-pattern stripe-pattern)
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
              b))))

    (define (gradient-pattern a b)
      (make-pattern
        (lambda (point)
          (color+ a
                  (color-scale
                    (color- b a)
                    (- (tuple-x point)
                       (floor (tuple-x point))))))))

    (define (ring-pattern a b)
      (make-pattern
        (lambda (p)
          (if (= 0 (remainder
                     (floor
                       (magnitude (tuple-sub p (point 0 0 0))))
                     2))
              a
              b))))

    (define (checkers-pattern a b)
      (make-pattern
        (lambda (point)
          (if (= 0 (remainder
                     (+ (floor (tuple-x point))
                        (floor (tuple-y point))
                        (floor (tuple-z point)))
                     2))
              a
              b))))))
