(define-library (raytrace tuple)
  (export tuple tuple? tuple-w tuple-x tuple-y tuple-z
          point point? vec vec?
          tuple-add tuple-sub tuple-neg tuple-scale tuple-div
          magnitude normalize dot cross
          color color?
          color-red color-green color-blue
          color+ color- color* color-scale color-clip color-round)
  (import (scheme base)
          (scheme write)
          (scheme inexact)
          (raytrace generic)
          (raytrace compare))
  (begin
    (define-record-type <tuple>
      (tuple x y z w)
      tuple?
      (x tuple-x)
      (y tuple-y)
      (z tuple-z)
      (w tuple-w))

    (define-record-type <color>
      (color r g b)
      color?
      (r color-red)
      (g color-green)
      (b color-blue))

    (define (point x y z) (tuple x y z 1.0))
    (define (point? obj) (and (tuple? obj) (= (tuple-w obj) 1.0)))

    (define (vec x y z) (tuple x y z 0.0))
    (define (vec? obj) (and (tuple? obj) (= (tuple-w obj) 0.0)))

    (define (tuple-add a b)
      (tuple (+ (tuple-x a) (tuple-x b))
             (+ (tuple-y a) (tuple-y b))
             (+ (tuple-z a) (tuple-z b))
             (+ (tuple-w a) (tuple-w b))))

    (define (tuple-sub a b)
      (tuple (- (tuple-x a) (tuple-x b))
             (- (tuple-y a) (tuple-y b))
             (- (tuple-z a) (tuple-z b))
             (- (tuple-w a) (tuple-w b))))

    (define (tuple-neg a)
      (tuple (- (tuple-x a))
             (- (tuple-y a))
             (- (tuple-z a))
             (- (tuple-w a))))

    (define (tuple-scale t s)
      (tuple (* (tuple-x t) s)
             (* (tuple-y t) s)
             (* (tuple-z t) s)
             (* (tuple-w t) s)))

    (define (tuple-div t s)
      (tuple-scale t (/ 1 s)))

    (define (magnitude t)
      (sqrt (dot t t)))

    (define (normalize t)
      (tuple-div t (magnitude t)))

    (define (dot a b)
      (+ (* (tuple-x a) (tuple-x b))
         (* (tuple-y a) (tuple-y b))
         (* (tuple-z a) (tuple-z b))
         (* (tuple-w a) (tuple-w b))))

    (define (cross a b)
      (let ((ax (tuple-x a)) (ay (tuple-y a)) (az (tuple-z a))
            (bx (tuple-x b)) (by (tuple-y b)) (bz (tuple-z b)))
        (vec (- (* ay bz) (* az by))
             (- (* az bx) (* ax bz))
             (- (* ax by) (* ay bx)))))

    (define (color+ a b)
      (color (+ (color-red a) (color-red b))
             (+ (color-green a) (color-green b))
             (+ (color-blue a) (color-blue b))))

    (define (color- a b)
      (color (- (color-red a) (color-red b))
             (- (color-green a) (color-green b))
             (- (color-blue a) (color-blue b))))

    (define (color* a b)
      (color (* (color-red a) (color-red b))
             (* (color-green a) (color-green b))
             (* (color-blue a) (color-blue b))))

    (define (color-scale c s)
      (color (* (color-red c) s)
             (* (color-green c) s)
             (* (color-blue c) s)))

    (define (color-clip min max c)
      (color (clip min max (color-red c))
             (clip min max (color-green c))
             (clip min max (color-blue c))))

    (define (color-round c)
      (color (exact (round (color-red c)))
             (exact (round (color-green c)))
             (exact (round (color-blue c)))))

    (define (clip min max x)
      (cond ((< x min) min)
            ((< max x) max)
            (else x)))

    (define (sqr x) (* x x))

    (define (tuple-almost-equal? a b)
      (and (almost= (tuple-x a) (tuple-x b))
           (almost= (tuple-y a) (tuple-y b))
           (almost= (tuple-z a) (tuple-z b))
           (almost= (tuple-w a) (tuple-w b))))

    (define (tuple-print obj)
      (display "[")
      (display (tuple-x obj))
      (display " ")
      (display (tuple-y obj))
      (display " ")
      (display (tuple-z obj))
      (display " ")
      (display (tuple-w obj))
      (display "]"))

    (define (tuple-dispatch method . args)
      (cond ((eq? 'print method) (apply tuple-print args))
            ((eq? 'almost-equal? method) (apply tuple-almost-equal? args))))

    (define (color-almost-equal? a b)
      (and (almost= (color-red a) (color-red b))
           (almost= (color-green a) (color-green b))
           (almost= (color-blue a) (color-blue b))))

    (define (color-print obj)
      (display "[r=")
      (display (color-red obj))
      (display " g=")
      (display (color-green obj))
      (display " b=")
      (display (color-blue obj))
      (display "]"))

    (define (color-dispatch method . args)
      (cond ((eq? 'print method) (apply color-print args))
            ((eq? 'almost-equal? method) (apply color-almost-equal? args))))

    (register-type tuple? tuple-dispatch)
    (register-type color? color-dispatch)))
