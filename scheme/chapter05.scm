(import (scheme base)
        (scheme write)
        (scheme file)
        (raytrace tuple)
        (raytrace canvas)
        (raytrace shapes)
        (raytrace ray)
        (raytrace matrix))

(define (cast ray scene)
  (cond ((null? scene) #f)
        ((null? (intersect (car scene) ray))
         (cast ray (cdr scene)))
        (else #t)))

(define scene (list (sphere)))
(define eye (point 0 0 -10))
(define focal-point -7)

(define (render c)
  (let ((w (canvas-width c))
        (h (canvas-height c)))
    (let loop ((x 0) (y 0))
      (cond ((= y h) c)
            ((= x w)
             (loop 0 (+ y 1)))
            (else
              (let* ((r (ray eye
                             (tuple-sub
                               (point (- (/ x w) 0.5)
                                      (- 0.5 (/ y h))
                                      focal-point)
                               eye)))
                     (color (if (cast r scene)
                                (color 0.8 0.0 0.0)
                                (color 0.0 0.2 0.2))))
                (pixel-set! c x y color)
                (loop (+ x 1) y)))))))

(define image (render (canvas 100 100)))

(call-with-output-file "chapter05.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
