(define-library (raytrace ray)
  (export intersect intersection intersection-object intersection-t
          intersections hit
          ray ray? ray-direction ray-origin ray-position ray-transform)
  (import (scheme base)
          (raytrace tuple)
          (raytrace matrix))
  (begin
    (define-record-type <ray>
      (ray origin direction)
      ray?
      (origin ray-origin)
      (direction ray-direction))

    (define (ray-position r t)
      (tuple-add (ray-origin r)
                 (tuple-scale (ray-direction r)
                              t)))

    (define (ray-transform r m)
      (ray (m4* m (ray-origin r))
           (m4* m (ray-direction r))))

    (define intersections list)

    (define (intersect obj ray)
      (obj 'intersect ray))

    (define intersection cons)
    (define intersection-t car)
    (define intersection-object cdr)

    (define (hit intersections)
      (let loop ((h #f)
                 (rest intersections))
        (cond ((null? rest) h)
              ((< (intersection-t (car rest)) 0)
               (loop h (cdr rest)))
              ((and h (< (intersection-t h)
                         (intersection-t (car rest))))
               (loop h (cdr rest)))
              (else (loop (car rest) (cdr rest))))))))
