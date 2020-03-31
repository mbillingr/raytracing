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
