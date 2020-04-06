(import (scheme base)
        (raytrace testing)
        (raytrace constants)
        (raytrace camera)
        (raytrace matrix)
        (raytrace tuple)
        (raytrace transformations)
        (raytrace ray)
        (raytrace world)
        (raytrace canvas))

(test "Constructing a camera"
  (given (hsize <- 160)
         (vsize <- 120)
         (field-of-view <- (/ PI 2)))
  (when (c <- (make-camera hsize vsize field-of-view)))
  (then ((c 'hsize) == hsize)
        ((c 'vsize) == vsize)
        ((c 'field-of-view) == field-of-view)
        ((c 'transform) == (identity-transform))))

(test "The pixel size for a horizontal canvas"
  (given (c <- (make-camera 200 125 (/ PI 2))))
  (then ((c 'pixel-size) == 0.01)))

(test "The pixel size for a vertical canvas"
  (given (c <- (make-camera 125 200 (/ PI 2))))
  (then ((c 'pixel-size) == 0.01)))

(test "Constructing a ray through the center of the canvas"
  (given (c <- (make-camera 201 101 (/ PI 2))))
  (when (r <- (c 'ray-for-pixel 100 50)))
  (then ((ray-origin r) == (point 0 0 0))
        ((ray-direction r) == (vec 0 0 -1))))

(test "Constructing a ray through the corner of the canvas"
  (given (c <- (make-camera 201 101 (/ PI 2))))
  (when (r <- (c 'ray-for-pixel 0 0)))
  (then ((ray-origin r) == (point 0 0 0))
        ((ray-direction r) == (vec 0.66519 0.33259 -0.66851))))

(test "Constructing a ray when the camera is transformed"
  (given (c <- (make-camera 201 101 (/ PI 2))))
  (when (c 'set-transform! (m4* (rotation-y (/ PI 4))
                                (translation 0 -2 5)))
        (r <- (c 'ray-for-pixel 100 50)))
  (then ((ray-origin r) == (point 0 2 -5))
        ((ray-direction r) == (vec SQRT2/2 0 (- SQRT2/2)))))

(test "Rendering a world with the camera"
  (given (w <- (default-world))
         (c <- (make-camera 11 11 (/ PI 2)))
         (from <- (point 0 0 -5))
         (to <- (point 0 0 0))
         (up <- (vec 0 1 0)))
  (when (c 'set-transform! (view-transform from to up))
        (image <- (c 'render w)))
  (then ((pixel-get image 5 5) == (color 0.38066 0.47583 0.2855))))
