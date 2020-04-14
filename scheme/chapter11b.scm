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

(define ground (plane))
(ground 'set-material! (make-material
                          (checkers-pattern (color 0.1 0.4 0.9)
                                            (color 0.1 0.2 0.5))
                          1.0 0.0 0.0 100.0 0.0 0.0 1.0))
(ground 'set-transform! (translation 0 -3 0))

(define water (plane))
(water 'set-material! (make-material
                         (color 0 0 0)
                         1.0 0.0 0.0 100.0 0.0 1.0 1.33))

(define foliage (sphere))
(foliage 'set-material! (make-material
                             (checkers-pattern (color 0.2 0.6 0.2)
                                               (color 0.6 0.4 0.1))
                             1.0 0.0 0.0 100.0 0.0 0.0 1.0))
((material-color (foliage 'material)) 'set-transform! (scaling 0.1 0.1 0.1))
(foliage 'set-transform! (scaling 100 100 100))

(define sky (plane))
(sky 'set-material! (make-material
                      (color 0.8 0.8 1)
                      1.0 0.0 0.0 100.0 0.0 0.0 1.0))
(sky 'set-transform! (translation 0 11 0))

(define world (make-world (list ground water sky foliage)
                          (list (point-light (point -10 10 -10)
                                             (color 1 1 1)))))

(define camera (make-camera 320 160 (/ PI 3)))
(camera 'set-transform! (view-transform (point 0 0.5 0)
                                        (point 0 0.5 1)
                                        (vec 0 1 0)))

(define image (camera 'render world))

(call-with-output-file "chapter11b.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
