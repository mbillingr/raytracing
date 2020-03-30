(import (scheme base)
        (scheme inexact)
        (raytrace testing)
        (raytrace matrix)
        (raytrace tuple))

(test "Constructing and inspecting a 4x4 matrix"
  (given (m <- (matrix ( 1    2    3    4)
                       ( 5.5  6.5  7.5  8.5)
                       ( 9   10   11   12)
                       (13.5 14.5 15.5 16.5))))
  (then ((m4-at m 0 0) == 1)
        ((m4-at m 0 3) == 4)
        ((m4-at m 1 0) == 5.5)
        ((m4-at m 1 2) == 7.5)
        ((m4-at m 2 2) == 11)
        ((m4-at m 3 0) == 13.5)
        ((m4-at m 3 2) == 15.5)))

(test "Constructing and inspecting a 2x2 matrix"
  (given (m <- (matrix (-3  5)
                       ( 1 -2))))
  (then ((m2-at m 0 0) == -3)
        ((m2-at m 0 1) == 5)
        ((m2-at m 1 0) == 1)
        ((m2-at m 1 1) == -2)))

(test "Constructing and inspecting a 3x3 matrix"
  (given (m <- (matrix (-3  5  0)
                       ( 1 -2 -7)
                       ( 0  1  1))))
  (then ((m3-at m 0 0) == -3)
        ((m3-at m 1 1) == -2)
        ((m3-at m 2 2) == 1)))

(test "Matrix equality with identical matrices"
  (given (A <- (matrix (1 2 3 4)
                       (5 6 7 8)
                       (9 8 7 6)
                       (5 4 3 2)))
         (B <- (matrix (1 2 3 4)
                       (5 6 7 8)
                       (9 8 7 6)
                       (5 4 3 2))))
  (then (A == B)))

(test "Matrix equality with different matrices"
  (given (A <- (matrix (1 2 3 4)
                       (5 6 7 8)
                       (9 8 7 6)
                       (5 4 3 2)))
         (B <- (matrix (2 3 4 5)
                       (6 7 8 9)
                       (8 7 6 5)
                       (4 3 2 1))))
  (then (A != B)))

(test "Multiplying two matrices"
  (given (A <- (matrix (1 2 3 4)
                       (5 6 7 8)
                       (9 8 7 6)
                       (5 4 3 2)))
         (B <- (matrix (-2 1 2  3)
                       ( 3 2 1 -1)
                       ( 4 3 6  5)
                       ( 1 2 7  8))))
  (then ((m4* A B) == (matrix ( 20  22  50  48)
                              ( 44  54 114 108)
                              ( 40  58 110 102)
                              ( 16  26  46  42)))))

(test "A matrix multiplied by a tuple"
  (given (A <- (matrix (1 2 3 4)
                       (2 4 4 2)
                       (8 6 4 1)
                       (0 0 0 1)))
         (b <- (tuple 1 2 3 1)))
  (then ((m4* A b) == (tuple 18 24 33 1))))

(test "Mupltiplying a matrix by the identity matrix"
  (given (A <- (matrix (0 1  2  4)
                       (1 2  4  8)
                       (2 4  8 16)
                       (4 8 16 32))))
  (then ((m4* A m4-identity) == A)))

(test "Mupltiplying the identity matrix by a tuple"
  (given (a <- (tuple 1 2 3 4)))
  (then ((m4* m4-identity a) == a)))

(test "Transposing a matrix"
  (given (A <- (matrix (0 9 3 0)
                       (9 8 0 8)
                       (1 8 5 3)
                       (0 0 5 8))))
  (then ((m4-transpose A) == (matrix (0 9 1 0)
                                     (9 8 8 0)
                                     (3 0 5 5)
                                     (0 8 3 8)))))

(test "Transposing the identity matrix"
  (given (A <- (m4-transpose m4-identity)))
  (then (A == m4-identity)))

(test "Calculating the determinant of a 2x2 matrix"
  (given (A <- (matrix ( 1 5)
                       (-3 2))))
  (then ((m2-determinant A) == 17)))

(test "A submatrix of a 3x3 matrix is a 2x2 matrix"
  (given (A <- (matrix ( 1 5  0)
                       (-3 2  7)
                       ( 0 6 -3))))
  (then ((m3-submatrix A 0 2) == (matrix (-3 2)
                                         ( 0 6)))))

(test "A submatrix of a 4x4 matrix is a 3x3 matrix"
  (given (A <- (matrix (-6 1  1 6)
                       (-8 5  8 6)
                       (-1 0  8 2)
                       (-7 1 -1 1))))
  (then ((m4-submatrix A 2 1) == (matrix (-6  1 6)
                                         (-8  8 6)
                                         (-7 -1 1)))))

(test "Calculating a minor of a 3x3 matrix"
  (given (A <- (matrix (3  5  0)
                       (2 -1 -7)
                       (6 -1  5)))
         (B <- (m3-submatrix A 1 0)))
  (then ((m3-minor A 1 0) == 25)
        ((m2-determinant B) == 25)))

(test "Calculating a cofactor of a 3x3 matrix"
  (given (A <- (matrix (3  5  0)
                       (2 -1 -7)
                       (6 -1  5))))
  (then ((m3-minor A 0 0) == -12)
        ((m3-cofactor A 0 0) == -12)
        ((m3-minor A 1 0) == 25)
        ((m3-cofactor A 1 0) == -25)))

(test "Calculating the determinant of a 3x3 matrix"
  (given (A <- (matrix ( 1 2  6)
                       (-5 8 -4)
                       ( 2 6  4))))
  (then ((m3-cofactor A 0 0) == 56)
        ((m3-cofactor A 0 1) == 12)
        ((m3-cofactor A 0 2) == -46)
        ((m3-determinant A) == -196)))

(test "Calculating the determinant of a 4x4 matrix"
  (given (A <- (matrix (-2 -8  3  5)
                       (-3  1  7  3)
                       ( 1  2 -9  6)
                       (-6  7  7 -9))))
  (then ((m4-cofactor A 0 0) == 690)
        ((m4-cofactor A 0 1) == 447)
        ((m4-cofactor A 0 2) == 210)
        ((m4-cofactor A 0 3) == 51)
        ((m4-determinant A) == -4071)))

(test "Testing an invertible matrix for invertibility"
  (given (A <- (matrix ( 6  4  4  4)
                       ( 5  5  7  6)
                       ( 4 -9  3 -7)
                       ( 9  1  7 -6))))
  (then ((m4-determinant A) == -2120)
        ((m4-invertible? A) == #t)))

(test "Testing a noninvertible matrix for invertibility"
  (given (A <- (matrix (-4  2 -2 -3)
                       ( 9  6  2  6)
                       ( 0 -5  1 -5)
                       ( 0  0  0  0))))
  (then ((m4-determinant A) == 0)
        ((m4-invertible? A) == #f)))

(test "Calculating the inverse of a matrix"
  (given (A <- (matrix (-5  2  6 -8)
                       ( 1 -5  1  8)
                       ( 7  7 -6 -7)
                       ( 1 -3  7  4)))
         (B <- (m4-inverse A)))
  (then ((m4-determinant A) == 532)
        ((m4-cofactor A 2 3) == -160)
        ((m4-at B 3 2) == (/ -160 532))
        ((m4-cofactor A 3 2) == 105)
        ((m4-at B 2 3) == (/ 105 532))
        (B == (matrix ( 0.21805  0.45113  0.24060 -0.04511)
                      (-0.80827 -1.45677 -0.44361  0.52068)
                      (-0.07895 -0.22368 -0.05263  0.19737)
                      (-0.52256 -0.81391 -0.30075  0.30639)))))

(test "Calculating the inverse of another matrix"
  (given (A <- (matrix ( 8 -5  9  2)
                       ( 7  5  6  1)
                       (-6  0  9  6)
                       (-3  0 -9 -4))))
  (then ((m4-inverse A) == (matrix (-0.15385 -0.15385 -0.28205 -0.53846)
                                   (-0.07692  0.12308  0.02564  0.03077)
                                   ( 0.35897  0.35897  0.43590  0.92308)
                                   (-0.69231 -0.69231 -0.76923 -1.92308)))))

(test "Calculating the inverse of a third matrix"
  (given (A <- (matrix ( 9  3  0  9)
                       (-5 -2 -6 -3)
                       (-4  9  6  4)
                       (-7  6  6  2))))
  (then ((m4-inverse A) == (matrix (-0.04074 -0.07778  0.14444 -0.22222)
                                   (-0.07778  0.03333  0.36667 -0.33333)
                                   (-0.02901 -0.14630 -0.10926  0.12963)
                                   ( 0.17778  0.06667 -0.26667  0.33333)))))

(test "Multiplying a product by its inverse"
  (given (A <- (matrix ( 3 -9  7  3)
                       ( 3 -8  2 -9)
                       (-4  4  4  1)
                       (-6  5 -1  1)))
         (B <- (matrix ( 8  2  2  2)
                       ( 3 -1  7  0)
                       ( 7  0  5  4)
                       ( 6 -2  0  5)))
         (C <- (m4* A B)))
  (then ((m4* C (m4-inverse B)) == A)))
