(define-library (raytrace tuple)
  (export tuple tuple? tuple-w tuple-x tuple-y tuple-z
          point point? vec vec?
          tuple-add tuple-sub tuple-neg tuple-scale tuple-div
          magnitude normalize dot cross
          print same?)
  (import (scheme base)
          (scheme write)
          (scheme inexact))
  (begin
    (define (tuple x y z w) (lambda (method) (method x y z w)))
    (define (point x y z) (tuple x y z 1.0))
    (define (vec x y z) (tuple x y z 0.0))

    (define (tuple? obj) (procedure? obj))
    (define (point? obj) (and (tuple? obj) (= (tuple-w obj) 1.0)))
    (define (vec? obj) (and (tuple? obj) (= (tuple-w obj) 0.0)))

    (define (tuple-x obj) (obj (lambda (x y z w) x)))
    (define (tuple-y obj) (obj (lambda (x y z w) y)))
    (define (tuple-z obj) (obj (lambda (x y z w) z)))
    (define (tuple-w obj) (obj (lambda (x y z w) w)))

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

    (define EPSILON 0.00001)

    (define (sqr x) (* x x))
    (define (abs x) (if (< x 0) (- x) x))

    (define (almost= a b)
      (< (abs (- a b))
         EPSILON))

    (define (same? a b)
      (cond ((and (tuple? a) (tuple? b))
             (and (almost= (tuple-x a) (tuple-x b))
                  (almost= (tuple-y a) (tuple-y b))
                  (almost= (tuple-z a) (tuple-z b))
                  (almost= (tuple-w a) (tuple-w b))))
            ((and (number? a) (number? b))
             (almost= a b))
            (else (equal? a b))))

    (define (print obj)
      (if (tuple? obj)
          (begin (display "[")
                 (display (tuple-x obj))
                 (display " ")
                 (display (tuple-y obj))
                 (display " ")
                 (display (tuple-z obj))
                 (display " ")
                 (display (tuple-w obj))
                 (display "]"))
          (display obj)))))
