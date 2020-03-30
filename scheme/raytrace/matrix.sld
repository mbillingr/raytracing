(define-library (raytrace matrix)
  (export matrix
          m3-at m3-cofactor m3-determinant m3-minor m3-submatrix
          m2-at m2-determinant
          m4-at m4* m4-cofactor m4-determinant m4-identity m4-inverse
          m4-invertible? m4-submatrix m4-transpose)
  (import (scheme base)
          (scheme write)
          (scheme inexact)
          (raytrace generic)
          (raytrace compare)
          (raytrace tuple))
  (begin
    (define <matrix-4x4> '<matrix-4x4>)
    (define <matrix-3x3> '<matrix-3x3>)
    (define <matrix-2x2> '<matrix-2x2>)

    (define (make-m4 m00 m01 m02 m03
                     m10 m11 m12 m13
                     m20 m21 m22 m23
                     m30 m31 m32 m33)
      (vector <matrix-4x4> m00 m01 m02 m03
                           m10 m11 m12 m13
                           m20 m21 m22 m23
                           m30 m31 m32 m33))

    (define (make-m3 m00 m01 m02
                     m10 m11 m12
                     m20 m21 m22)
      (vector <matrix-3x3> m00 m01 m02
                           m10 m11 m12
                           m20 m21 m22))

    (define (make-m2 m00 m01 m10 m11)
      (vector <matrix-2x2> m00 m01 m10 m11))

    (define m4-identity
      (make-m4 1 0 0 0
               0 1 0 0
               0 0 1 0
               0 0 0 1))

    (define (m4-at m i j)
      (vector-ref m (+ 1 j (* i 4))))

    (define (m3-at m i j)
      (vector-ref m (+ 1 j (* i 3))))

    (define (m2-at m i j)
      (vector-ref m (+ 1 j (* i 2))))

    (define (m4-set! m i j x)
      (vector-set! m (+ 1 j (* i 4)) x))

    (define (m3-set! m i j x)
      (vector-set! m (+ 1 j (* i 3)) x))

    (define (m2-set! m i j x)
      (vector-set! m (+ 1 j (* i 2)) x))

    (define (m4* a b)
      (cond ((tuple? b)
             (mul-m4-tuple a b))
            (else (m4-mul a b))))

    (define (m4-mul a b)
      (let ((m (make-m4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
        (let loop-row ((i 0))
          (let loop-col ((j 0))
            (m4-set! m i j (+ (* (m4-at a i 0) (m4-at b 0 j))
                              (* (m4-at a i 1) (m4-at b 1 j))
                              (* (m4-at a i 2) (m4-at b 2 j))
                              (* (m4-at a i 3) (m4-at b 3 j))))
            (if (< j 3) (loop-col (+ j 1))))
          (if (< i 3) (loop-row (+ i 1))))
        m))

    (define (mul-m4-tuple a b)
      (tuple (+ (* (m4-at a 0 0) (tuple-x b))
                (* (m4-at a 0 1) (tuple-y b))
                (* (m4-at a 0 2) (tuple-z b))
                (* (m4-at a 0 3) (tuple-w b)))
             (+ (* (m4-at a 1 0) (tuple-x b))
                (* (m4-at a 1 1) (tuple-y b))
                (* (m4-at a 1 2) (tuple-z b))
                (* (m4-at a 1 3) (tuple-w b)))
             (+ (* (m4-at a 2 0) (tuple-x b))
                (* (m4-at a 2 1) (tuple-y b))
                (* (m4-at a 2 2) (tuple-z b))
                (* (m4-at a 2 3) (tuple-w b)))
             (+ (* (m4-at a 3 0) (tuple-x b))
                (* (m4-at a 3 1) (tuple-y b))
                (* (m4-at a 3 2) (tuple-z b))
                (* (m4-at a 3 3) (tuple-w b)))))

    (define (m4-transpose m)
      (make-m4 (m4-at m 0 0) (m4-at m 1 0) (m4-at m 2 0) (m4-at m 3 0)
               (m4-at m 0 1) (m4-at m 1 1) (m4-at m 2 1) (m4-at m 3 1)
               (m4-at m 0 2) (m4-at m 1 2) (m4-at m 2 2) (m4-at m 3 2)
               (m4-at m 0 3) (m4-at m 1 3) (m4-at m 2 3) (m4-at m 3 3)))

    (define (m4-invertible? m)
      (not (eq? (m4-determinant m) 0)))

    (define (m4-inverse m)
      (let ((d (m4-determinant m))
            (c m4-cofactor))
        (make-m4
          (/ (c m 0 0) d) (/ (c m 1 0) d) (/ (c m 2 0) d) (/ (c m 3 0) d)
          (/ (c m 0 1) d) (/ (c m 1 1) d) (/ (c m 2 1) d) (/ (c m 3 1) d)
          (/ (c m 0 2) d) (/ (c m 1 2) d) (/ (c m 2 2) d) (/ (c m 3 2) d)
          (/ (c m 0 3) d) (/ (c m 1 3) d) (/ (c m 2 3) d) (/ (c m 3 3) d))))

    (define (m4-determinant m)
      (+ (* (m4-at m 0 0) (m4-cofactor m 0 0))
         (* (m4-at m 0 1) (m4-cofactor m 0 1))
         (* (m4-at m 0 2) (m4-cofactor m 0 2))
         (* (m4-at m 0 3) (m4-cofactor m 0 3))))

    (define (m4-cofactor m i j)
      (if (= 0 (remainder (+ i j) 2))
          (m4-minor m i j)
          (- (m4-minor m i j))))

    (define (m4-minor m i j)
      (m3-determinant (m4-submatrix m i j)))

    (define (m3-determinant m)
      (+ (* (m3-at m 0 0) (m3-cofactor m 0 0))
         (* (m3-at m 0 1) (m3-cofactor m 0 1))
         (* (m3-at m 0 2) (m3-cofactor m 0 2))))

    (define (m3-cofactor m i j)
      (if (= 0 (remainder (+ i j) 2))
          (m3-minor m i j)
          (- (m3-minor m i j))))

    (define (m3-minor m i j)
      (m2-determinant (m3-submatrix m i j)))

    (define (m2-determinant m)
      (- (* (vector-ref m 1) (vector-ref m 4))
         (* (vector-ref m 2) (vector-ref m 3))))

    (define (m3-submatrix m i0 j0)
      (let ((s (make-m2 0 0 0 0)))
        (let loop ((i 0) (j 0)
                   (k 0) (l 0))
          (cond ((= i i0) (loop (+ i 1) j k l))
                ((= j j0) (loop i (+ j 1) k l))
                ((= i 3) s)
                ((= j 3) (loop (+ i 1) 0 (+ k 1) 0))
                (else (m2-set! s k l (m3-at m i j))
                      (loop i (+ j 1) k (+ l 1)))))))

    (define (m4-submatrix m i0 j0)
      (let ((s (make-m3 0 0 0 0 0 0 0 0 0)))
        (let loop ((i 0) (j 0)
                   (k 0) (l 0))
          (cond ((= i i0) (loop (+ i 1) j k l))
                ((= j j0) (loop i (+ j 1) k l))
                ((= i 4) s)
                ((= j 4) (loop (+ i 1) 0 (+ k 1) 0))
                (else (m3-set! s k l (m4-at m i j))
                      (loop i (+ j 1) k (+ l 1)))))))

    (define-syntax matrix
      (syntax-rules ()
        ((matrix (m00 m01 m02 m03)
                 (m10 m11 m12 m13)
                 (m20 m21 m22 m23)
                 (m30 m31 m32 m33))
         (make-m4 m00 m01 m02 m03
                  m10 m11 m12 m13
                  m20 m21 m22 m23
                  m30 m31 m32 m33))
        ((matrix (m00 m01 m02)
                 (m10 m11 m12)
                 (m20 m21 m22))
         (make-m3 m00 m01 m02
                  m10 m11 m12
                  m20 m21 m22))
        ((matrix (m00 m01)
                 (m10 m11))
         (make-m2 m00 m01 m10 m11))))

    (define (matrix? obj)
      (and (vector? obj)
           (let ((type (matrix-type obj)))
             (or (eq? type <matrix-4x4>)
                 (eq? type <matrix-3x3>)
                 (eq? type <matrix-2x2>)))))

    (define (matrix-type obj)
      (vector-ref obj 0))

    (define (matrix-print obj)
      (display obj))

    (define (vector-almost-equal? a b)
      (let* ((na (vector-length a))
             (nb (vector-length b))
             (n (if (< na nb) na nb)))
        (let loop ((i (- n 1)))
          (if (< i 0)
              #t
              (and (almost-equal? (vector-ref a i)
                                  (vector-ref b i))
                   (loop (- i 1)))))))

    (define (matrix-dispatch method . args)
      (cond ((eq? 'print method) (apply matrix-print args))
            ((eq? 'almost-equal? method) (apply vector-almost-equal? args))))

    (register-type matrix? matrix-dispatch)))
