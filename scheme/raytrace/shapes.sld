(define-library (raytrace shapes)
  (export make-shape sphere plane)
  (import (scheme base)
          (scheme inexact)
          (raytrace tuple)
          (raytrace ray)
          (raytrace matrix)
          (raytrace transformations)
          (raytrace material)
          (raytrace constants)
          (raytrace compare))
  (begin
    (define (sphere)
      (make-shape (sphere-geometry)))

    (define (plane)
      (make-shape (plane-geometry)))

    (define (make-shape geometry)
      (define transform (identity-transform))
      (define inv-transform (identity-transform))
      (define material (default-material))

      (define (intersect r)
        (geometry 'intersect dispatch (ray-transform r inv-transform)))

      (define (normal-at world-p)
        (let* ((obj-p (m4* inv-transform world-p))
               (obj-n (geometry 'normal-at obj-p))
               (world-n (m4* (m4-transpose inv-transform)
                             obj-n)))
          (tuple-set-w! world-n 0)
          (normalize world-n)))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'transform) transform)
              ((eq? m 'material) material)
              ((eq? m 'set-transform!)
               (set! transform (car args))
               (set! inv-transform (m4-inverse (car args))))
              ((eq? m 'set-material!) (set! material (car args)))
              (else (apply geometry m args))))

      dispatch)

    (define (sphere-geometry)
      (define (intersect shape ray2)
        (let* ((sphere-to-ray (tuple-sub (ray-origin ray2)
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
                  shape)
                (intersection
                  (/ (- b (sqrt discriminant))
                     (* -2 a))
                  shape)))))

      (define (normal-at obj-p)
        (tuple-sub obj-p (point 0 0 0)))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              (else (error "unknown method (sphere-geometry m ...)" m))))

      dispatch)

    (define (plane-geometry)
      (define (intersect shape local-ray)
        (if (almost= 0 (tuple-y (ray-direction local-ray)))
            (intersections)
            (intersections
              (intersection (/ (- (tuple-y (ray-origin local-ray)))
                               (tuple-y (ray-direction local-ray)))
                            shape))))

      (define (normal-at local-p)
        (vec 0 1 0))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              (else (error "unknown method (plane-geometry m ...)" m))))

      dispatch)))
