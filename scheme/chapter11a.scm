(import (scheme base)
        (scheme write)
        (scheme file)
        (raytrace tuple)
        (raytrace canvas)
        (raytrace shapes)
        (raytrace matrix)
        (raytrace lights)
        (raytrace material)
        (raytrace transformations)
        (raytrace constants)
        (raytrace camera)
        (raytrace world)
        (raytrace pattern))

(define ground-material (make-material
                          (checkers-pattern (color 0.8 0.2 0.2)
                                            (color 0.2 0.2 0.8))
                          1.0 0.0 0.0 100.0 0.0 0.0 1.0))

(define ceiling-material (make-material
                           (color 0.8 0.8 1)
                           1.0 0.0 0.0 100.0 0.0 0.0 1.0))

(define ground (plane))
(ground 'set-material! ground-material)

(define ceiling (plane))
(ceiling 'set-material! ceiling-material)
(ceiling 'set-transform! (translation 0 11 0))

(define left (sphere))
(left 'set-transform! (translation -1.5 1 0.5))
(left 'set-material! (make-material
                       (color 0 0 0)
                       0.0 0.0 1.0 200.0 1.0 0.0 1.0))

(define right (sphere))
(right 'set-transform! (translation 1.5 1 0.5))
(right 'set-material! (make-material
                        (color 0 0 0)
                        0.0 0.0 1.0 200.0 0.0 1.0 1.5))

(define right-inner (sphere))
(right-inner 'set-transform! (m4* (translation 1.5 1 0.5) (scaling 0.5 0.5 0.5)))
(right-inner 'set-material! (make-material
                              (color 0 0 0)
                              0.0 0.0 0.0 200.0 0.0 1.0 0.8))

(define world (make-world (list ground ceiling
                                right right-inner
                                left)
                          (list (point-light (point -10 10 -10)
                                             (color 1 1 1)))))

(define camera (make-camera 320 160 (/ PI 3)))
(camera 'set-transform! (view-transform (point 0 1.5 -5)
                                        (point 0 1 0)
                                        (vec 0 1 0)))

(define image (camera 'render world))

(call-with-output-file "chapter11a.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
