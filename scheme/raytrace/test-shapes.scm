(import (scheme base) (scheme write)
        (raytrace testing)
        (raytrace ray)
        (raytrace shapes)
        (raytrace tuple)
        (raytrace matrix)
        (raytrace transformations)
        (raytrace constants)
        (raytrace material))

;; generic shapes
;; ===========================================================================

(define (test-shape)
  (make-shape (test-geometry)))

(define (test-geometry)
  (define last-ray #f)
  (define (intersect shape r) (set! last-ray r))
  (define (normal-at p) (vec (tuple-x p) (tuple-y p) (tuple-z p)))
  (define (dispatch m . args)
    (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
          ((eq? m 'normal-at) (normal-at (car args)))
          ((eq? m 'last-ray) last-ray)
          (else (error "unknown method (test-geometry m ...)" m))))
  dispatch)

(test "The default transformation"
  (given (s <- (test-shape)))
  (then ((s 'transform) == (identity-transform))))

(test "Assigning a transformation"
  (given (s <- (test-shape)))
  (when (s 'set-transform! (translation 2 3 4)))
  (then ((s 'transform) == (translation 2 3 4))))

(test "A shape has a default material"
  (given (s <- (test-shape)))
  (when (m <- (s 'material)))
  (then (m == (default-material))))

(test "A shape may be assigned a material"
  (given (s <- (test-shape))
         (m <- (material (color 1 0 0) 1 1 0 200)))
  (when (s 'set-material! m))
  (then ((s 'material) == m)))

(test "Intersecting a scaled shape with a ray"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (s <- (test-shape)))
  (when (s 'set-transform! (scaling 2 2 2))
        (xs <- (s 'intersect r)))
  (then ((ray-origin (s 'last-ray)) == (point 0 0 -2.5))
        ((ray-direction (s 'last-ray)) == (vec 0 0 0.5))))

(test "Intersecting a translated shape with a ray"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (s <- (test-shape)))
  (when (s 'set-transform! (translation 5 0 0))
        (xs <- (s 'intersect r)))
  (then ((ray-origin (s 'last-ray)) == (point -5 0 -5))
        ((ray-direction (s 'last-ray)) == (vec 0 0 1))))

(test "Computing the normal on a translated shape"
  (given (s <- (test-shape)))
  (when (s 'set-transform! (translation 0 1 0))
        (n <- (s 'normal-at (point 0 1.70711 0.70711))))
  (then (n == (vec 0 0.70711 0.70711))))

(test "Computing the normal on a transformed shape"
  (given (s <- (test-shape))
         (m <- (m4* (scaling 1 0.5 1) (rotation-z (/ PI 5)))))
  (when (s 'set-transform! m)
        (n <- (s 'normal-at (point 0 SQRT2/2 (- SQRT2/2)))))
  (then (n == (vec 0 0.97014 -0.24254))))

;; sphere shape
;; ===========================================================================

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

(test "The normal on a sphere at a point on the x axis"
  (given (s <- (sphere)))
  (when (n <- (s 'normal-at (point 1 0 0))))
  (then (n == (vec 1 0 0))))

(test "The normal on a sphere at a point on the y axis"
  (given (s <- (sphere)))
  (when (n <- (s 'normal-at (point 0 -1 0))))
  (then (n == (vec 0 -1 0))))

(test "The normal on a sphere at a point on the z axis"
  (given (s <- (sphere)))
  (when (n <- (s 'normal-at (point 0 0 1))))
  (then (n == (vec 0 0 1))))

(test "The normal on a sphere at a nonaxial point"
  (given (s <- (sphere)))
  (when (n <- (s 'normal-at (point SQRT3/3 SQRT3/3 SQRT3/3))))
  (then (n == (vec SQRT3/3 SQRT3/3 SQRT3/3))))

(test "The normal is a normalized vector"
  (given (s <- (sphere)))
  (when (n <- (s 'normal-at (point SQRT3/3 SQRT3/3 SQRT3/3))))
  (then (n == (normalize n))))

(test "Computing the normal on a translated sphere"
  (given (s <- (sphere)))
  (when (s 'set-transform! (translation 0 1 0))
        (n <- (s 'normal-at (point 0 1.70711 -0.70711))))
  (then (n == (vec 0 0.70711 -0.70711))))

(test "Computing the normal on a transformed sphere"
  (given (s <- (sphere)))
  (when (s 'set-transform! (m4* (scaling 1 0.5 1) (rotation-z (/ PI 5))))
        (n <- (s 'normal-at (point 0 SQRT2/2 (- SQRT2/2)))))
  (then (n == (vec 0 0.97014 -0.24254))))

(test "The default material"
  (given (m <- (default-material)))
  (then ((material-color m) == (color 1 1 1))
        ((material-ambient m) == 0.1)
        ((material-diffuse m) == 0.9)
        ((material-specular m) == 0.9)
        ((material-shininess m) == 200)))

(test "A sphere has a default material"
  (given (s <- (sphere)))
  (when (m <- (s 'material)))
  (then (m == (default-material))))

(test "A sphere may be assigned a material"
  (given (s <- (sphere))
         (m <- (material (color 1 0 0) 1 1 0 200)))
  (when (s 'set-material! m))
  (then ((s 'material) == m)))
