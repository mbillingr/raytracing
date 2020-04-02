(import (scheme base)
        (raytrace testing)
        (raytrace ray)
        (raytrace tuple)
        (raytrace shapes)
        (raytrace transformations))

(test "Creating and querying a ray"
  (given (origin <- (point 1 2 3))
         (direction <- (point 4 5 6)))
  (when (r <- (ray origin direction)))
  (then ((ray-origin r) == origin)
        ((ray-direction r) == direction)))

(test "Computing a point from a distance"
  (given (r <- (ray (point 2 3 4) (vec 1 0 0))))
  (then ((ray-position r 0) == (point 2 3 4))
        ((ray-position r 1) == (point 3 3 4))
        ((ray-position r -1) == (point 1 3 4))
        ((ray-position r 2.5) == (point 4.5 3 4))))

(test "An intersection encapsulates t and object"
  (given (s <- (vector 'dummy-shape)))
  (when (i <- (intersection 3.5 s)))
  (then ((intersection-t i) == 3.5)
        ((intersection-object i) == s)))

(test "Aggregating intersections"
  (given (s <- (vector 'dummy-shape))
         (i1 <- (intersection 1 s))
         (i2 <- (intersection 2 s)))
  (when (xs <- (intersections i1 i2)))
  (then ((length xs) == 2)
        ((intersection-t (car xs)) == 1)
        ((intersection-t (cadr xs)) == 2)))

(test "Intersect sets the object on intersection"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (s <- (sphere)))
  (when (xs <- (intersect s r)))
  (then ((length xs) == 2)
        ((intersection-object (car xs)) == s)
        ((intersection-object (cadr xs)) == s)))

(test "The hit, when all intersections have positive t"
  (given (s <- (sphere))
         (i1 <- (intersection 1 s))
         (i2 <- (intersection 2 s))
         (xs <- (intersections i2 i1)))
  (when (i <- (hit xs)))
  (then (i == i1)))

(test "The hit, when some intersections have negative t"
  (given (s <- (sphere))
         (i1 <- (intersection -1 s))
         (i2 <- (intersection 1 s))
         (xs <- (intersections i2 i1)))
  (when (i <- (hit xs)))
  (then (i == i2)))

(test "The hit, when all intersections have negative t"
  (given (s <- (sphere))
         (i1 <- (intersection -2 s))
         (i2 <- (intersection -1 s))
         (xs <- (intersections i2 i1)))
  (when (i <- (hit xs)))
  (then (i == #f)))

(test "The hit is always the lowest nonnegative intersection"
  (given (s <- (sphere))
         (i1 <- (intersection 5 s))
         (i2 <- (intersection 7 s))
         (i3 <- (intersection -3 s))
         (i4 <- (intersection 2 s))
         (xs <- (intersections i1 i2 i3 i4)))
  (when (i <- (hit xs)))
  (then (i == i4)))

(test "Translating a ray"
  (given (r <- (ray (point 1 2 3) (vec 0 1 0)))
         (m <- (translation 3 4 5)))
  (when (r2 <- (ray-transform r m)))
  (then ((ray-origin r2) == (point 4 6 8))
        ((ray-direction r2) == (vec 0 1 0))))

(test "Scaling a ray"
  (given (r <- (ray (point 1 2 3) (vec 0 1 0)))
         (m <- (scaling 2 3 4)))
  (when (r2 <- (ray-transform r m)))
  (then ((ray-origin r2) == (point 2 6 12))
        ((ray-direction r2) == (vec 0 3 0))))
