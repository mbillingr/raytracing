(import (scheme base)
        (scheme write)
        (scheme file)
        (raytrace tuple)
        (raytrace canvas))

(define-record-type <projectile>
  (make-projectile pos vel)
  projectile?
  (pos projectile-pos projectile-set-pos!)
  (vel projectile-vel projectile-set-vel!))

(define-record-type <environment>
  (make-environment gravity wind)
  environment?
  (gravity environment-gravity environment-set-gravity!)
  (wind environment-wind environment-set-wind!))

(define (tick e p)
  (make-projectile
    (tuple-add (projectile-pos p)
               (projectile-vel p))
    (tuple-add (projectile-vel p)
               (tuple-add (environment-gravity e)
                         (environment-wind e)))))

(define (run c e p color)
    (let loop ((p p))
      (if (< (tuple-y (projectile-pos p)) 0)
          c
          (let ((x (exact (round (tuple-x (projectile-pos p)))))
                (y (- (canvas-height c)
                      1
                      (exact (round (tuple-y (projectile-pos p)))))))
            (pixel-set! c x y color)
            (loop (tick e p))))))

(define env (make-environment (vec 0 -0.04 0) (vec -0.01 0 0)))

(define projectile1 (make-projectile (point 0 0 0) (vec 1 1 0)))
(define projectile2 (make-projectile (point 0 0 0) (vec 0.5 1.5 0)))
(define projectile3 (make-projectile (point 0 0 0) (vec 1.5 0.5 0)))

(define image (run (canvas 64 48) env projectile1 (color 1 0.5 0.5)))
(define image2 (run image env projectile2 (color 0.5 1.0 0.5)))
(define image (run image2 env projectile3 (color 0.5 0.5 1.0)))

(call-with-output-file "chapter02.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
