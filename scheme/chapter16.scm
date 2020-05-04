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
        (raytrace csg))

(define floor-material (material (color 1 0.9 0.9)
                                 0.3 0.9 0.0 100.0))

(define floor (plane))
(floor 'set-material! floor-material)

(define a (cube))
(a 'set-material! (material (color 0.2 0.5 1) 0.3 0.9 0.2 50))
(define b (sphere))
(b 'set-material! (material (color 1 0.5 0.2) 0.3 0.9 0.2 50))
(b 'set-transform! (translation 0.5 0.5 -0.5))
(define c (csg csg-difference a b))
(c 'set-transform! (translation 0 1 0))

(define world (make-world (list floor c)
                          (list (point-light (point -8 10 -9)
                                             (color 1 1 1)))))

(define camera (make-camera 320 160 (/ PI 3)))
(camera 'set-transform! (view-transform (point -1.5 4 -5)
                                        (point 0 1 0)
                                        (vec 0 1 0)))

(define image (camera 'render world))

(call-with-output-file "chapter16.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
