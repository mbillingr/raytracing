(import (scheme base)
        (raytrace testing)
        (raytrace tuple)
        (raytrace pattern)
        (raytrace material)
        (raytrace lights)
        (raytrace shapes)
        (raytrace transformations)
        (raytrace matrix))

(define BLACK (color 0 0 0))
(define WHITE (color 1 1 1))

;; generic patterns
;; ===========================================================================

(define (test-pattern)
  (make-pattern (lambda (p) (color (tuple-x p) (tuple-y p) (tuple-z p)))))

(test "The default pattern transformation"
  (given (pattern <- (test-pattern)))
  (then ((pattern 'inv-transform) == (identity-transform))))

(test "Assigning a transformation"
  (given (pattern <- (test-pattern)))
  (when (pattern 'set-transform! (translation 1 2 3)))
  (then ((pattern 'inv-transform) == (m4-inverse (translation 1 2 3)))))

(test "A pattern with an object transformation"
  (given (shape <- (sphere))
         (pattern <- (test-pattern)))
  (when (shape 'set-transform! (scaling 2 2 2))
        (c <- (shape 'pattern-at pattern (point 2 3 4))))
  (then (c == (color 1 1.5 2))))

(test "A pattern with a pattern transformation"
  (given (shape <- (sphere))
         (pattern <- (test-pattern)))
  (when (pattern 'set-transform! (scaling 2 2 2))
        (c <- (shape 'pattern-at pattern (point 2 3 4))))
  (then (c == (color 1 1.5 2))))

(test "A pattern with both an object and a pattern transformation"
  (given (shape <- (sphere))
         (pattern <- (test-pattern)))
  (when (pattern 'set-transform! (translation 0.5 1 1.5))
        (shape 'set-transform! (scaling 2 2 2))
        (c <- (shape 'pattern-at pattern (point 2.5 3 3.5))))
  (then (c == (color 0.75 0.5 0.25))))

;; stripe pattern
;; ===========================================================================

(test "A stripe pattern is constant in y"
  (given (pattern <- (stripe-pattern WHITE BLACK)))
  (then ((pattern 'at (point 0 0 0)) == WHITE)
        ((pattern 'at (point 0 1 0)) == WHITE)
        ((pattern 'at (point 0 2 0)) == WHITE)))

(test "A stripe pattern is constant in z"
  (given (pattern <- (stripe-pattern WHITE BLACK)))
  (then ((pattern 'at (point 0 0 0)) == WHITE)
        ((pattern 'at (point 0 0 1)) == WHITE)
        ((pattern 'at (point 0 0 2)) == WHITE)))

(test "A stripe pattern alternates in x"
  (given (pattern <- (stripe-pattern WHITE BLACK)))
  (then ((pattern 'at (point 0.0 0 0)) == WHITE)
        ((pattern 'at (point 0.9 0 0)) == WHITE)
        ((pattern 'at (point 1.0 0 0)) == BLACK)
        ((pattern 'at (point -0.1 0 0)) == BLACK)
        ((pattern 'at (point -1.0 0 0)) == BLACK)
        ((pattern 'at (point -1.1 0 0)) == WHITE)))

(test "Lighting with a pattern applied"
  (given (m <- (material (stripe-pattern WHITE BLACK)
                         1 0 0 1.0))
         (eyev <- (vec 0 0 -1))
         (normalv <- (vec 0 0 -1))
         (light <- (point-light (point 0 0 -10) WHITE))
         (s <- (sphere)))
  (when (c1 <- (lighting m s light (point 0.9 0 0) eyev normalv #f))
        (c2 <- (lighting m s light (point 1.1 0 0) eyev normalv #f)))
  (then (c1 == WHITE)
        (c2 == BLACK)))
