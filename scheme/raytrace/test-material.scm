(import (scheme base) (scheme write)
        (raytrace testing)
        (raytrace ray)
        (raytrace tuple)
        (raytrace matrix)
        (raytrace lights)
        (raytrace constants)
        (raytrace material))

(let ((m (default-material))
      (pos (point 0 0 0)))

  (test "Lighting with the eye between light and surface"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 -10) (color 1 1 1))))
    (when (result <- (lighting m light pos eyev normalv)))
    (then (result == (color 1.9 1.9 1.9))))

  (test "Lighting with the eye between light and surface, eye offset 45 deg"
    (given (eyev <- (vec 0 SQRT2/2 (- SQRT2/2)))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 -10) (color 1 1 1))))
    (when (result <- (lighting m light pos eyev normalv)))
    (then (result == (color 1.0 1.0 1.0))))

  (test "Lighting with eye opposite surface, light offset 45 deg"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 10 -10) (color 1 1 1))))
    (when (result <- (lighting m light pos eyev normalv)))
    (then (result == (color 0.7364 0.7364 0.7364))))

  (test "Lighting with eye in the path of the reflection vector"
    (given (eyev <- (vec 0 (- SQRT2/2) (- SQRT2/2)))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 10 -10) (color 1 1 1))))
    (when (result <- (lighting m light pos eyev normalv)))
    (then (result == (color 1.6364 1.6364 1.6364))))

  (test "Lighting with the light behind the surface"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 10) (color 1 1 1))))
    (when (result <- (lighting m light pos eyev normalv)))
    (then (result == (color 0.1 0.1 0.1)))))
