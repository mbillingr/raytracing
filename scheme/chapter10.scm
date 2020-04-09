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

(define ground-material (material (stripe-pattern (color 0.5 0.5 0.5) (color 1 0.9 0.9))
                                  0.1 0.9 0.0 100.0))

(define ground (plane))
(ground 'set-material! ground-material)

(define middle (sphere))
(middle 'set-transform! (translation -0.5 1 0.5))
(middle 'set-material! (material (stripe-pattern (color 0.5 0.5 0.5) (color 0.1 1 0.5))
                                0.1 0.7 0.3 200.0))

(define right (sphere))
(right 'set-transform! (m4* (translation 1.5 0.5 -0.5)
                            (scaling 0.5 0.5 0.5)))
(right 'set-material! (material (stripe-pattern (color 0.5 0.5 0.5) (color 0.5 1 0.1))
                               0.1 0.7 0.3 200.0))

(define left (sphere))
(left 'set-transform! (m4* (translation -1.5 0.33 -0.75)
                           (scaling 0.33 0.33 0.33)))
(left 'set-material! (material (stripe-pattern (color 0.5 0.5 0.5) (color 1 0.8 0.1))
                              0.1 0.7 0.3 200.0))

(define world (make-world (list ground
                                middle
                                right
                                left)
                          (list (point-light (point -10 10 -10)
                                             (color 1 1 1)))))

(define camera (make-camera 320 160 (/ PI 3)))
(camera 'set-transform! (view-transform (point 0 1.5 -5)
                                        (point 0 1 0)
                                        (vec 0 1 0)))

(define image (camera 'render world))

(call-with-output-file "chapter10.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
