(import (scheme base) (scheme write)
        (raytrace testing)
        (raytrace ray)
        (raytrace tuple)
        (raytrace matrix)
        (raytrace lights)
        (raytrace constants)
        (raytrace material)
        (raytrace shapes))

(let ((m (default-material))
      (pos (point 0 0 0)))

  (test "Lighting with the eye between light and surface"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 -10) (color 1 1 1))))
    (when (result <- (lighting m (sphere) light pos eyev normalv #f)))
    (then (result == (color 1.9 1.9 1.9))))

  (test "Lighting with the eye between light and surface, eye offset 45 deg"
    (given (eyev <- (vec 0 SQRT2/2 (- SQRT2/2)))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 -10) (color 1 1 1))))
    (when (result <- (lighting m (sphere) light pos eyev normalv #f)))
    (then (result == (color 1.0 1.0 1.0))))

  (test "Lighting with eye opposite surface, light offset 45 deg"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 10 -10) (color 1 1 1))))
    (when (result <- (lighting m (sphere) light pos eyev normalv #f)))
    (then (result == (color 0.7364 0.7364 0.7364))))

  (test "Lighting with eye in the path of the reflection vector"
    (given (eyev <- (vec 0 (- SQRT2/2) (- SQRT2/2)))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 10 -10) (color 1 1 1))))
    (when (result <- (lighting m (sphere) light pos eyev normalv #f)))
    (then (result == (color 1.6364 1.6364 1.6364))))

  (test "Lighting with the light behind the surface"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 10) (color 1 1 1))))
    (when (result <- (lighting m (sphere) light pos eyev normalv #f)))
    (then (result == (color 0.1 0.1 0.1))))

  (test "Lighting with the surface in shadow"
    (given (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 -10) (color 1 1 1))))
    (when (result <- (lighting m (sphere) light pos eyev normalv #t)))
    (then (result == (color 0.1 0.1 0.1))))

  (test "Reflectivity for the default material"
    (given (m <- (default-material)))
    (then ((material-reflective m) == 0)))

  (test "Transparency and Refractive Index for the default material"
    (given (m <- (default-material)))
    (then ((material-transparency m) == 0)
          ((material-refractive-index m) == 1))))
