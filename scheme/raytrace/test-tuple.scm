(import (scheme base)
        (scheme write)
        (scheme inexact)
        (raytrace tuple))

(define-syntax test
  (syntax-rules (given then <-)
    ((test description
       (given (var <- val) ...)
       (then (a == b) ...))
     (test description
       (given (var <- val) ...)
       (when)
       (then (a == b) ...)))

    ((test description
       (given (var <- val) ...)
       (when (var2 <- val2) ...)
       (then (a == b) ...))
     (let ((var val) ...)
       (let ((var2 val2) ...)
         (let ((a-val a)
               (b-val b))
           (if (same? a-val b-val)
               'pass
               (begin
                 (display "FAIL: ")
                 (display description)
                 (newline)
                 (display "    ")
                 (display 'a)
                 (display " == ")
                 (display 'b)
                 (newline)
                 (display "    ")
                 (print a-val)
                 (display " == ")
                 (print b-val)
                 (newline)
                 (error description `(same? a b)))))
         ...
         (display "PASS: ")
         (display description)
         (newline))))))

(test "A tuple with w=1.0 is a point"
  (given (a <- (tuple 4.3 -4.2 3.1 1.0)))
  (then ((tuple-x a) == 4.3)
        ((tuple-y a) == -4.2)
        ((tuple-z a) == 3.1)
        ((tuple-w a) == 1.0)
        ((point? a) == #t)
        ((vec? a) == #f)))

(test "A tuple with w=0 is a vector"
  (given (a <- (tuple 4.3 -4.2 3.1 0.0)))
  (then ((tuple-x a) == 4.3)
        ((tuple-y a) == -4.2)
        ((tuple-z a) == 3.1)
        ((tuple-w a) == 0.0)
        ((point? a) == #f)
        ((vec? a) == #t)))

(test "point() creates tuples with w=1"
  (given (p <- (point 4 -4 3)))
  (then (p == (tuple 4 -4 3 1))))

(test "vec() creates tuples with w=0"
  (given (v <- (vec 4 -4 3)))
  (then (v == (tuple 4 -4 3 0))))

(test "Adding two tuples"
  (given (a1 <- (tuple 3 -2 5 1))
         (a2 <- (tuple -2 3 1 0)))
  (then ((tuple-add a1 a2) == (tuple 1 1 6 1))))

(test "Subtracting two tuples"
  (given (p1 <- (point 3 2 1))
         (p2 <- (point 5 6 7)))
  (then ((tuple-sub p1 p2) == (vec -2 -4 -6))))

(test "Subtracting two vectors"
  (given (v1 <- (vec 3 2 1))
         (v2 <- (vec 5 6 7)))
  (then ((tuple-sub v1 v2) == (vec -2 -4 -6))))

(test "Subtracting a vector from the zero vector"
  (given (zero <- (vec 0 0 0))
         (v <- (vec 1 -2 3)))
  (then ((tuple-sub zero v) == (vec -1 2 -3))))

(test "Negating a tuple"
  (given (a <- (tuple 1 -2 3 -4)))
  (then ((tuple-neg a) == (tuple -1 2 -3 4))))

(test "Multiplying a tuple by a scalar"
  (given (a <- (tuple 1 -2 3 -4)))
  (then ((tuple-scale a 3.5) == (tuple 3.5 -7 10.5 -14))))

(test "Multiplying a tuple by a fraction"
  (given (a <- (tuple 1 -2 3 -4)))
  (then ((tuple-scale a 0.5) == (tuple 0.5 -1 1.5 -2))))

(test "Dividing a tuple by a scalar"
  (given (a <- (tuple 1 -2 3 -4)))
  (then ((tuple-div a 2) == (tuple 0.5 -1 1.5 -2))))

(test "Computing the magnitude of (vec 1 0 0)"
  (given (a <- (vec 1 0 0)))
  (then ((magnitude a) == 1)))

(test "Computing the magnitude of (vec 0 1 0)"
  (given (a <- (vec 0 1 0)))
  (then ((magnitude a) == 1)))

(test "Computing the magnitude of (vec 0 0 1)"
  (given (a <- (vec 0 0 1)))
  (then ((magnitude a) == 1)))

(test "Computing the magnitude of (vec 1 2 3)"
  (given (a <- (vec 1 2 3)))
  (then ((magnitude a) == (sqrt 14))))

(test "Computing the magnitude of (vec -1 -2 -3)"
  (given (a <- (vec -1 -2 -3)))
  (then ((magnitude a) == (sqrt 14))))

(test "Normalizing (vec 4 0 0) gives (1 0 0)"
  (given (v <- (vec 4 0 0)))
  (then ((normalize v) == (vec 1 0 0))))

(test "Normalizing (vec 1 2 3)"
  (given (v <- (vec 1 2 3)))
  (then ((normalize v) == (vec (/ 1 (sqrt 14)) (/ 2 (sqrt 14)) (/ 3 (sqrt 14))))))

(test "The magnitude of a normalized vector"
  (given (v <- (vec 1 2 3)))
  (when (norm <- (normalize v)))
  (then ((magnitude norm) == 1)))

(test "The dot product of two tuples"
  (given (a <- (vec 1 2 3))
         (b <- (vec 2 3 4)))
  (then ((dot a b) == 20)))

(test "The cross product of two vectors"
  (given (a <- (vec 1 2 3))
         (b <- (vec 2 3 4)))
  (then ((cross a b) == (vec -1 2 -1))
        ((cross b a) == (vec 1 -2 1))))
