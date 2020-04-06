(import (scheme base)
        (scheme write)
        (scheme file)
        (raytrace tuple)
        (raytrace canvas)
        (raytrace shapes)
        (raytrace ray)
        (raytrace matrix)
        (raytrace lights)
        (raytrace material))

(define (cast ray scene light)
  (if (null? scene)
      (color 0 0 0)
      (let ((xs (intersect (car scene) ray)))
        (if (null? xs)
            (cast ray (cdr scene) light)
            (begin
              (let* ((h (hit xs))
                     (t (intersection-t h))
                     (mat ((intersection-object h) 'material))
                     (point (ray-position ray t))
                     (eyev (tuple-neg (ray-direction ray)))
                     (normalv ((intersection-object h) 'normal-at point)))
                (lighting mat light point eyev normalv #f)))))))

(define light (point-light (point 1 9 -10) (color 1 1 1)))
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
                             (normalize (tuple-sub
                                          (point (- (/ x w) 0.5)
                                                 (- 0.5 (/ y h))
                                                 focal-point)
                                          eye))))
                     (color (cast r scene light)))
                (pixel-set! c x y color)
                (loop (+ x 1) y)))))))

(define image (render (canvas 200 200)))

(call-with-output-file "chapter06.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
