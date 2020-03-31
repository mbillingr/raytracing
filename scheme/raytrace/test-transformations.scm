(import (scheme base)
        (raytrace testing)
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
