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

(test "A helper for producing a sphere with a glassy material"
  (given (s <- (glass-sphere)))
  (then ((s 'transform) == (identity-transform))
        ((material-transparency (s 'material)) == 1)
        ((material-refractive-index (s 'material)) == 1.5)))

;; plane shape
;; ===========================================================================

(test "The normal of a plane is constant everywhere"
  (given (p <- (plane)))
  (when (n1 <- (p 'normal-at (point 0 0 0)))
        (n2 <- (p 'normal-at (point 10 0 -10)))
        (n3 <- (p 'normal-at (point -5 0 150))))
  (then (n1 == (vec 0 1 0))
        (n2 == (vec 0 1 0))
        (n3 == (vec 0 1 0))))

(test "Intersect with a ray parallel to the plane"
  (given (p <- (plane))
         (r <- (ray (point 0 10 0) (vec 0 0 1))))
  (when (xs <- (p 'intersect r)))
  (then (xs == '())))

(test "Intersect plane with a coplanar ray"
  (given (p <- (plane))
         (r <- (ray (point 0 0 0) (vec 0 0 1))))
  (when (xs <- (p 'intersect r)))
  (then (xs == '())))

(test "A ray intersecting a plane from above"
  (given (p <- (plane))
         (r <- (ray (point 0 1 0) (vec 0 -1 0))))
  (when (xs <- (p 'intersect r)))
  (then (xs == `((1.0 . ,p)))))

(test "A ray intersecting a plane from below"
  (given (p <- (plane))
         (r <- (ray (point 0 -1 0) (vec 0 1 0))))
  (when (xs <- (p 'intersect r)))
  (then (xs == `((1.0 . ,p)))))

;; cube shape
;; ===========================================================================

(let ((outline
        (lambda (origin direction t1 t2)
          (test "A ray intersects a cube"
            (given (c <- (cube))
                   (r <- (ray origin direction)))
            (when (xs <- (c 'intersect r)))
            (then ((length xs) == 2)
                  ((intersection-t (car xs)) == t1)
                  ((intersection-t (cadr xs)) == t2))))))
  (outline (point  5 0.5 0) (vec -1 0 0) 4 6)   ; +x
  (outline (point -5 0.5 0) (vec  1 0 0) 4 6)   ; -x
  (outline (point 0.5  5 0) (vec 0 -1 0) 4 6)   ; +y
  (outline (point 0.5 -5 0) (vec 0  1 0) 4 6)   ; -y
  (outline (point 0.5 0  5) (vec 0 0 -1) 4 6)   ; +z
  (outline (point 0.5 0 -5) (vec 0 0  1) 4 6)   ; -z
  (outline (point 0 0.5 0) (vec 0 0 1) -1 1))   ; inside

(let ((outline
        (lambda (origin direction)
          (test "A ray misses a cube"
            (given (c <- (cube))
                   (r <- (ray origin direction)))
            (when (xs <- (c 'intersect r)))
            (then ((length xs) == 0))))))
  (outline (point -2 0 0) (vec 0.2673 0.5345 0.8018))
  (outline (point 0 -2 0) (vec 0.8018 0.2673 0.5345))
  (outline (point 0 0 -2) (vec 0.5345 0.8018 0.2673))
  (outline (point 2 0 2) (vec 0 0 -1))
  (outline (point 0 2 2) (vec 0 -1 0))
  (outline (point 2 2 0) (vec -1 0 0)))

(test "Normals on the surfaces of a cube"
  (given (c <- (cube)))
  (then ((c 'normal-at (point  1.0  0.5 -0.8)) == (vec  1 0 0))
        ((c 'normal-at (point -1.0 -0.2  0.9)) == (vec -1 0 0))
        ((c 'normal-at (point -0.4  1.0 -0.1)) == (vec 0  1 0))
        ((c 'normal-at (point  0.3 -1.0 -0.7)) == (vec 0 -1 0))
        ((c 'normal-at (point -0.6  0.3  1.0)) == (vec 0 0  1))
        ((c 'normal-at (point  0.4  0.4 -1.0)) == (vec 0 0 -1))
        ((c 'normal-at (point 1 1 1)) == (vec 1 0 0))
        ((c 'normal-at (point -1 -1 -1)) == (vec -1 0 0))))

;; cylinder shape
;; ===========================================================================

(let ((outline
        (lambda (origin direction)
          (test "A ray misses a cylinder"
            (given (cyl <- (cylinder))
                   (direction <- (normalize direction))
                   (r <- (ray origin direction)))
            (when (xs <- (cyl 'intersect r)))
            (then ((length xs) == 0))))))
  (outline (point 1 0 0) (vec 0 1 0))
  (outline (point 0 0 0) (vec 0 1 0))
  (outline (point 0 0 -5) (vec 1 1 1)))

(let ((outline
        (lambda (origin direction t0 t1)
          (test "A ray strikes a cylinder"
            (given (cyl <- (cylinder))
                   (direction <- (normalize direction))
                   (r <- (ray origin direction)))
            (when (xs <- (cyl 'intersect r)))
            (then ((length xs) == 2)
                  ((intersection-t (car xs)) == t0)
                  ((intersection-t (cadr xs)) == t1))))))
  (outline (point 1 0 -5) (vec 0 0 1) 5 5)
  (outline (point 0 0 -5) (vec 0 0 1) 4 6)
  (outline (point 0.5 0 -5) (vec 0.1 1 1) 6.80798 7.08872))

(let ((outline
        (lambda (point normal)
          (test "Normal vector on a cylinder"
            (given (cyl <- (cylinder))
                   (n <- (cyl 'normal-at point)))
            (then (n == normal))))))
  (outline (point 1 0 0) (vec 1 0 0))
  (outline (point 0 5 -1) (vec 0 0 -1))
  (outline (point 0 -2 1) (vec 0 0 1))
  (outline (point -1 1 0) (vec -1 0 0)))

(test "The default minimum and maximum for a cylinder"
  (given (cyl <- (cylinder)))
  (then ((cyl 'minimum) == -inf.0)
        ((cyl 'maximum) == +inf.0)))

(let ((outline
        (lambda (origin direction count)
          (test "Intersecting a constrained cylinder"
            (given (cyl <- (cylinder))
                   (r <- (ray origin (normalize direction))))
            (when (cyl 'set-minimum! 1)
                  (cyl 'set-maximum! 2)
                  (xs <- (cyl 'intersect r)))
            (then ((length xs) == count))))))
  (outline (point 0 1.5 0) (vec 0.1 1 0) 0)
  (outline (point 0 3 -5) (vec 0 0 1) 0)
  (outline (point 0 0 -5) (vec 0 0 1) 0)
  (outline (point 0 2 -5) (vec 0 0 1) 0)
  (outline (point 0 1 -5) (vec 0 0 1) 0)
  (outline (point 0 1.5 -2) (vec 0 0 1) 2))

(test "The default closed value for a cylinder"
  (given (cyl <- (cylinder)))
  (then ((cyl 'closed?) == #f)))

(let ((outline
        (lambda (origin direction count)
          (test "Intersecting a closed cylinder"
            (given (cyl <- (cylinder))
                   (r <- (ray origin (normalize direction))))
            (when (cyl 'set-minimum! 1)
                  (cyl 'set-maximum! 2)
                  (cyl 'set-closed! #t)
                  (xs <- (cyl 'intersect r)))
            (then ((length xs) == count))))))
  (outline (point 0 3 0) (vec 0 -1 0) 2)
  (outline (point 0 3 -2) (vec 0 -1 2) 2)
  (outline (point 0 4 -2) (vec 0 -1 1) 2)  ; corner case
  (outline (point 0 0 -2) (vec 0 1 2) 2)
  (outline (point 0 -1 -2) (vec 0 1 1) 2)) ; corner case

(let ((outline
        (lambda (point normal)
          (test "Normal vector on a cylinder's end caps"
            (given (cyl <- (cylinder)))
            (when (cyl 'set-minimum! 1)
                  (cyl 'set-maximum! 2)
                  (cyl 'set-closed! #t)
                  (n <- (cyl 'normal-at point)))
            (then (n == normal))))))
  (outline (point 0 1 0) (vec 0 -1 0))
  (outline (point 0.5 1 0) (vec 0 -1 0))
  (outline (point 0 1 0.5) (vec 0 -1 0))
  (outline (point 0 2 0) (vec 0 1 0))
  (outline (point 0.5 2 0) (vec 0 1 0))
  (outline (point 0 2 0.5) (vec 0 1 0)))

;; cone shape
;; ===========================================================================

(let ((outline
        (lambda (origin direction t0 t1)
          (test "Intersecting a cone with a ray"
            (given (shape <- (cone))
                   (direction <- (normalize direction))
                   (r <- (ray origin direction)))
            (when (xs <- (shape 'intersect r)))
            (then ((length xs) == 2)
                  ((intersection-t (car xs)) == t0)
                  ((intersection-t (cadr xs)) == t1))))))
  (outline (point 0 0 -5) (vec 0 0 1) 5 5)
  (outline (point 0 0 -5) (vec 1 1 1) 8.66025 8.66025)
  (outline (point 1 1 -5) (vec -0.5 -1 1) 4.55006 49.44994))

(test "Intersecting a cone with a ray parallel to one of its halves"
  (given (shape <- (cone))
         (direction <- (normalize (vec 0 1 1)))
         (r <- (ray (point 0 0 -1) direction)))
  (when (xs <- (shape 'intersect r)))
  (then ((length xs) == 1)
        ((intersection-t (car xs)) == 0.35355)))

(let ((outline
        (lambda (origin direction count)
          (test "Intersecting a cone's end caps"
            (given (shape <- (cone))
                   (r <- (ray origin (normalize direction))))
            (when (shape 'set-minimum! -0.5)
                  (shape 'set-maximum! 0.5)
                  (shape 'set-closed! #t)
                  (xs <- (shape 'intersect r)))
            (then ((length xs) == count))))))
  (outline (point 0 0 -5) (vec 0 1 0) 0)
  (outline (point 0 0 -0.25) (vec 0 1 1) 2)
  (outline (point 0 0 -0.25) (vec 0 1 0) 4))

(let ((outline
        (lambda (point normal)
          (test "Normal vector on a cone"
            (given (g <- (cone-geometry)))
            (when (n <- (g 'normal-at point)))
            (then (n == normal))))))
  (outline (point 0 0 0) (vec 0 0 0))
  (outline (point 1 1 1) (vec 1 (- SQRT2) 1))
  (outline (point -1 -1 0) (vec -1 1 0)))

;; shape group
;; ===========================================================================

(test "Creating a new group"
  (given (g <- (group)))
  (then ((g 'transform) == (identity-transform))
        ((g 'shapes) == '())))

(test "A shape has a parent attribute"
  (given (s <- (test-shape)))
  (then ((s 'parent) == #f)))

(test "Adding a child to a group"
  (given (g <- (group))
         (s <- (test-shape)))
  (when (g 'add-children! s))
  (then ((g 'transform) == (identity-transform))
        ((g 'shapes) == (list s))
        ((s 'parent) == g)))

(test "Intersecting a ray with an empty group"
  (given (g <- (group))
         (r <- (ray (point 0 0 0) (vec 0 0 1)))
         (xs <- (g 'local-intersect r)))
  (then ((length xs) == 0)))

(test "Intersecting a ray with a nonempty group"
  (given (g <- (group))
         (s1 <- (sphere))
         (s2 <- (sphere))
         (s3 <- (sphere))
         (r <- (ray (point 0 0 0) (vec 0 0 1))))
  (when (s2 'set-transform! (translation 0 0 -3))
        (s3 'set-transform! (translation 5 0 0))
        (g 'add-children! s1 s2 s3)
        (xs <- (g 'local-intersect r)))
  (then ((length xs) == 4)
        ((intersection-object (car xs)) == s2)
        ((intersection-object (cadr xs)) == s2)
        ((intersection-object (car (cddr xs))) == s1)
        ((intersection-object (cadr (cddr xs))) == s1)))

(test "Intersecting a transformed group"
  (given (g <- (group))
         (s <- (sphere))
         (r <- (ray (point 10 0 -10) (vec 0 0 1))))
  (when (g 'set-transform! (scaling 2 2 2))
        (s 'set-transform! (translation 5 0 0))
        (g 'add-children! s)
        (xs <- (g 'intersect r)))
  (then ((length xs) == 2)))

(test "Converting a point from world to object space"
  (given (g1 <- (group))
         (g2 <- (group))
         (s <- (sphere))
         (r <- (ray (point 10 0 -10) (vec 0 0 1))))
  (when (g1 'set-transform! (rotation-y (/ PI 2)))
        (g2 'set-transform! (scaling 2 2 2))
        (s 'set-transform! (translation 5 0 0))
        (g1 'add-children! g2)
        (g2 'add-children! s))
  (then ((s 'world-to-object (point -2 0 -10)) == (point 0 0 -1))))

(test "Converting a normal from object to world space"
  (given (g1 <- (group))
         (g2 <- (group))
         (s <- (sphere))
         (r <- (ray (point 10 0 -10) (vec 0 0 1))))
  (when (g1 'set-transform! (rotation-y (/ PI 2)))
        (g2 'set-transform! (scaling 1 2 3))
        (s 'set-transform! (translation 5 0 0))
        (g1 'add-children! g2)
        (g2 'add-children! s)
        (n <- (s 'normal-to-world (vec SQRT3/3 SQRT3/3 SQRT3/3))))
  (then (n == (vec 0.28571 0.42857 -0.85714))))

(test "Finding the normal on a child object"
  (given (g1 <- (group))
         (g2 <- (group))
         (s <- (sphere))
         (r <- (ray (point 10 0 -10) (vec 0 0 1))))
  (when (g1 'set-transform! (rotation-y (/ PI 2)))
        (g2 'set-transform! (scaling 1 2 3))
        (s 'set-transform! (translation 5 0 0))
        (g1 'add-children! g2)
        (g2 'add-children! s)
        (n <- (s 'normal-at (point 1.7321 1.1547 -5.5774))))
  (then (n == (vec 0.28570 0.42854 -0.85716))))
