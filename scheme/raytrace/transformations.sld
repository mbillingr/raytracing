(define-library (raytrace transformations)
  (export rotation-x rotation-y rotation-z
          scaling shearing translation
          identity-transform)
  (import (scheme base)
          (scheme inexact)
          (raytrace matrix))
  (begin
    (define (identity-transform)
      m4-identity)

    (define (translation dx dy dz)
      (matrix (1 0 0 dx)
              (0 1 0 dy)
              (0 0 1 dz)
              (0 0 0 1)))

    (define (scaling sx sy sz)
      (matrix (sx  0  0 0)
              ( 0 sy  0 0)
              ( 0  0 sz 0)
              ( 0  0  0 1)))

    (define (rotation-x phi)
      (matrix (1 0 0 0)
              (0 (cos phi) (- (sin phi)) 0)
              (0 (sin phi) (cos phi) 0)
              (0 0 0 1)))

    (define (rotation-y phi)
      (matrix ((cos phi) 0 (sin phi) 0)
              (0 1 0 0)
              ((- (sin phi)) 0 (cos phi) 0)
              (0 0 0 1)))

    (define (rotation-z phi)
      (matrix ((cos phi) (- (sin phi)) 0 0)
              ((sin phi) (cos phi) 0 0)
              (0 0 1 0)
              (0 0 0 1)))

    (define (shearing xy xz yx yz zx zy)
      (matrix ( 1 xy xz 0)
              (yx  1 yz 0)
              (zx zy  1 0)
              ( 0  0  0 1)))))
