(import (scheme base)
        (raytrace testing)
        (raytrace tuple)
        (raytrace pattern)
        (raytrace material)
        (raytrace lights))

(let ((black (color 0 0 0))
      (white (color 1 1 1)))

  (test "Creating a stripe pattern"
    (given (pattern <- (stripe-pattern white black)))
    (then ((pattern 'a) == white)
          ((pattern 'b) == black)))

  (test "A stripe pattern is constant in y"
    (given (pattern <- (stripe-pattern white black)))
    (then ((pattern 'at (point 0 0 0)) == white)
          ((pattern 'at (point 0 1 0)) == white)
          ((pattern 'at (point 0 2 0)) == white)))

  (test "A stripe pattern is constant in z"
    (given (pattern <- (stripe-pattern white black)))
    (then ((pattern 'at (point 0 0 0)) == white)
          ((pattern 'at (point 0 0 1)) == white)
          ((pattern 'at (point 0 0 2)) == white)))

  (test "A stripe pattern alternates in x"
    (given (pattern <- (stripe-pattern white black)))
    (then ((pattern 'at (point 0.0 0 0)) == white)
          ((pattern 'at (point 0.9 0 0)) == white)
          ((pattern 'at (point 1.0 0 0)) == black)
          ((pattern 'at (point -0.1 0 0)) == black)
          ((pattern 'at (point -1.0 0 0)) == black)
          ((pattern 'at (point -1.1 0 0)) == white)))

  (test "Lighting with a pattern applied"
    (given (m <- (material (stripe-pattern white black)
                           1 0 0 1.0))
           (eyev <- (vec 0 0 -1))
           (normalv <- (vec 0 0 -1))
           (light <- (point-light (point 0 0 -10) white)))
    (when (c1 <- (lighting m light (point 0.9 0 0) eyev normalv #f))
          (c2 <- (lighting m light (point 1.1 0 0) eyev normalv #f)))
    (then (c1 == white)
          (c2 == black))))
