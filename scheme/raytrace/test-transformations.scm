(import (scheme base)
        (raytrace testing)
        (raytrace constants)
        (raytrace matrix)
        (raytrace tuple)
        (raytrace transformations))

(test "Multiplying by a translation matrix"
  (given (transform <- (translation 5 -3 2))
         (p <- (point -3 4 5)))
  (then ((m4* transform p) == (point 2 1 7))))

(test "Multiplying by the inverse of a translation matrix"
  (given (transform <- (translation 5 -3 2))
         (inv <- (m4-inverse transform))
         (p <- (point -3 4 5)))
  (then ((m4* inv p) == (point -8 7 3))))

(test "Translation does not affect vectors"
  (given (transform <- (translation 5 -3 2))
         (v <- (vec -3 4 5)))
  (then ((m4* transform v) == (vec -3 4 5))))

(test "A scaling matrix applied to a point"
  (given (transform <- (scaling 2 3 4))
         (p <- (point -4 6 8)))
  (then ((m4* transform p) == (point -8 18 32))))

(test "A scaling matrix applied to a vector"
  (given (transform <- (scaling 2 3 4))
         (v <- (vec -4 6 8)))
  (then ((m4* transform v) == (vec -8 18 32))))

(test "Multiplying by the inverse of a scaling matrix"
  (given (transform <- (scaling 2 3 4))
         (inv <- (m4-inverse transform))
         (p <- (point -4 6 8)))
  (then ((m4* inv p) == (point -2 2 2))))

(test "Reflection is scaling by a negative value"
  (given (transform <- (scaling -1 1 1))
         (p <- (point 2 3 4)))
  (then ((m4* transform p) == (point -2 3 4))))

(test "Rotating a point around the x axis"
  (given (p <- (point 0 1 0))
         (half_quarter <- (rotation-x (/ PI 4)))
         (full_quarter <- (rotation-x (/ PI 2))))
  (then ((m4* half_quarter p) == (point 0 (/ SQRT2 2) (/ SQRT2 2)))
        ((m4* full_quarter p) == (point 0 0 1))))

(test "The inverse of an x-rotation rotates in the opposite direction"
  (given (p <- (point 0 1 0))
         (half-quarter <- (rotation-x (/ PI 4)))
         (inv <- (m4-inverse half-quarter)))
  (then ((m4* inv p) == (point 0 (/ SQRT2 2) (/ SQRT2 -2)))))

(test "Rotating a point around the y axis"
  (given (p <- (point 0 0 1))
         (half-quarter <- (rotation-y (/ PI 4)))
         (full-quarter <- (rotation-y (/ PI 2))))
  (then ((m4* half-quarter p) == (point (/ SQRT2 2) 0 (/ SQRT2 2)))
        ((m4* full-quarter p) == (point 1 0 0))))

(test "Rotating a point around the z axis"
  (given (p <- (point 0 1 0))
         (half-quarter <- (rotation-z (/ PI 4)))
         (full-quarter <- (rotation-z (/ PI 2))))
  (then ((m4* half-quarter p) == (point (/ SQRT2 -2) (/ SQRT2 2) 0))
        ((m4* full-quarter p) == (point -1 0 0))))
