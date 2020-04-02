(import (scheme base) (scheme write)
        (raytrace testing)
        (raytrace ray)
        (raytrace shapes)
        (raytrace tuple)
        (raytrace matrix)
        (raytrace transformations))

(test "A ray intersects a sphere at two points"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (xs <- (intersect s r)))
  (then (xs == `((4.0 . ,s) (6.0 . ,s)))))

(test "A ray intersects a sphere at a tangent"
  (given (r <- (ray (point 0 1 -5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (xs <- (intersect s r)))
  (then (xs == `((5.0 . ,s) (5.0 . ,s)))))

(test "A ray misses a sphere"
  (given (r <- (ray (point 0 2 -5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (xs <- (intersect s r)))
  (then (xs == '())))

(test "A ray originates inside a sphere"
  (given (r <- (ray (point 0 0 0) (vec 0 0 1)))
         (s <- (sphere)))
  (when (xs <- (intersect s r)))
  (then (xs == `((-1.0 . ,s) (1.0 . ,s)))))

(test "A sphere is behind a ray"
  (given (r <- (ray (point 0 0 5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (xs <- (intersect s r)))
  (then (xs == `((-6.0 . ,s) (-4.0 . ,s)))))

(test "A sphere's default transformation"
  (given (s <- (sphere)))
  (then ((s 'transform) == m4-identity)))

(test "Changing a sphere's transformation"
  (given (s <- (sphere))
         (t <- (translation 2 3 4)))
  (when (s 'set-transform! t))
  (then ((s 'transform) == t)))

(test "Intersecting a scaled sphere with a ray"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (s 'set-transform! (scaling 2 2 2))
        (xs <- (intersect s r)))
  (then (xs == `((3.0 . ,s) (7.0 . ,s)))))

(test "Intersecting a translated sphere with a ray"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (s 'set-transform! (translation 5 0 0))
        (xs <- (intersect s r)))
  (then (xs == '())))
