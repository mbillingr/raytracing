(define-library (raytrace shapes)
  (export sphere)
  (import (scheme base)
          (scheme inexact)
          (raytrace tuple)
          (raytrace ray)
          (raytrace matrix)
          (raytrace transformations))
  (begin

    (define (sphere)
      (define transform (identity-transform))

      (define (intersect r)
        (let* ((ray2 (ray-transform r (m4-inverse transform)))
               (sphere-to-ray (tuple-sub (ray-origin ray2)
                                         (point 0 0 0)))
               (dir (ray-direction ray2))
               (a (dot dir dir))
               (b (* 2 (dot dir
                            sphere-to-ray)))
               (c (- (dot sphere-to-ray
                          sphere-to-ray)
                     1))
               (discriminant (- (* b b)
                                (* 4 a c))))
          (if (< discriminant 0)
              '()
              (intersections
                (intersection
                  (/ (+ b (sqrt discriminant))
                     (* -2 a))
                  dispatch)
                (intersection
                  (/ (- b (sqrt discriminant))
                     (* -2 a))
                  dispatch)))))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args)))
              ((eq? m 'transform) transform)
              ((eq? m 'set-transform!) (set! transform (car args)))
              (else (error "unknown method (sphere m ...)" m))))

      dispatch)))
