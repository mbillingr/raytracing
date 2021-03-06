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
        (raytrace world))

(define floor-material (material (color 1 0.9 0.9)
                                 0.1 0.9 0.0 100.0))

(define floor (sphere))
(floor 'set-transform! (scaling 10 0.01 10))
(floor 'set-material! floor-material)

(define left-wall (sphere))
(left-wall 'set-transform! (m4* (m4* (translation 0 0 5)
                                     (rotation-y (- (/ PI 4))))
                                (m4* (rotation-x (- (/ PI 2)))
                                     (scaling 10 0.01 10))))
(left-wall 'set-material! floor-material)

(define right-wall (sphere))
(right-wall 'set-transform! (m4* (m4* (translation 0 0 5)
                                      (rotation-y (/ PI 4)))
                                 (m4* (rotation-x (/ PI 2))
                                      (scaling 10 0.01 10))))
(right-wall 'set-material! floor-material)

(define middle (sphere))
(middle 'set-transform! (translation -0.5 1 0.5))
(middle 'set-material! (material (color 0.1 1 0.5)
                                0.1 0.7 0.3 200.0))

(define right (sphere))
(right 'set-transform! (m4* (translation 1.5 0.5 -0.5)
                            (scaling 0.5 0.5 0.5)))
(right 'set-material! (material (color 0.5 1 0.1)
                               0.1 0.7 0.3 200.0))

(define left (sphere))
(left 'set-transform! (m4* (translation -1.5 0.33 -0.75)
                           (scaling 0.33 0.33 0.33)))
(left 'set-material! (material (color 1 0.8 0.1)
                              0.1 0.7 0.3 200.0))

(define world (make-world (list floor
                                left-wall
                                right-wall
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

(call-with-output-file "chapter07.ppm"
  (lambda (f)
    (display (canvas->ppm image) f)))
