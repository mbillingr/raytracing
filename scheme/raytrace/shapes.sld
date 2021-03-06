(define-library (raytrace shapes)
  (export make-shape  glass-sphere
          group
          cone cone-geometry
          cube cube-geometry
          cylinder cylinder-geometry
          plane plane-geometry
          sphere sphere-geometry)
  (import (scheme base) (scheme write)
          (scheme inexact)
          (raytrace tuple)
          (raytrace ray)
          (raytrace matrix)
          (raytrace transformations)
          (raytrace material)
          (raytrace constants)
          (raytrace compare)
          (raytrace aabb))
  (begin
    (define (group)
      (make-group))

    (define (sphere)
      (make-shape (sphere-geometry)))

    (define (plane)
      (make-shape (plane-geometry)))

    (define (cube)
      (make-shape (cube-geometry)))

    (define (cylinder)
      (make-shape (cylinder-geometry)))

    (define (cone)
      (make-shape (cone-geometry)))

    (define (glass-sphere)
      (let ((s (make-shape (sphere-geometry))))
        (material-set-transparency! (s 'material) 1)
        (material-set-refractive-index! (s 'material) 1.5)
        s))

    (define (make-shape geometry)
      (define transform (identity-transform))
      (define inv-transform (identity-transform))
      (define material (default-material))
      (define parent #f)

      (define (intersect r)
        (geometry 'intersect dispatch (ray-transform r inv-transform)))

      (define (normal-at world-p)
        (normal-to-world
          (geometry 'normal-at
                    (world-to-object world-p))))

      (define (pattern-at pattern world-p)
        (pattern 'at (world-to-object world-p)))

      (define (world-to-object point)
        (m4* inv-transform
             (if parent
                 (parent 'world-to-object point)
                 point)))

      (define (normal-to-world normal)
        (let ((n (m4* (m4-transpose inv-transform) normal)))
          (tuple-set-w! n 0)
          (set! n (normalize n))
          (if parent
              (parent 'normal-to-world n)
              n)))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'transform) transform)
              ((eq? m 'material) material)
              ((eq? m 'pattern-at) (pattern-at (car args) (cadr args)))
              ((eq? m 'world-to-object) (world-to-object (car args)))
              ((eq? m 'normal-to-world) (normal-to-world (car args)))
              ((eq? m 'local-intersect) (geometry 'intersect dispatch (car args)))
              ((eq? m 'contains) (or (eq? (car args) dispatch)
                                     (geometry 'contains (car args))))
              ((eq? m 'parent) parent)
              ((eq? m 'set-transform!)
               (set! transform (car args))
               (set! inv-transform (m4-inverse (car args))))
              ((eq? m 'set-material!) (set! material (car args)))
              ((eq? m 'set-parent!) (set! parent (car args)))
              ((or (eq? m 'aabb)
                   (eq? m 'update-aabb!))
               (aabb-transform (geometry 'aabb) transform))
              (else (apply geometry m args))))

      dispatch)

    (define (make-group)
      (define transform (identity-transform))
      (define inv-transform (identity-transform))
      (define shapes '())
      (define parent #f)
      (define aabb #f)

      (define (add-children child*)
        (if (null? child*)
            'done
            (let ((child (car child*)))
              (child 'set-parent! dispatch)
              (set! shapes (cons child shapes))
              (add-children (cdr child*)))))

      (define (intersect r)
        (if (or (not aabb)
                (aabb-intersect aabb r))
            (local-intersect (ray-transform r inv-transform))
            (intersections)))

      (define (local-intersect r)
        (let loop ((s shapes)
                   (xs (intersections)))
          (if (null? s)
              xs
              (loop (cdr s)
                    (merge-intersections xs ((car s) 'intersect r))))))

      (define (world-to-object point)
        (m4* inv-transform
             (if parent
                 (parent 'world-to-object point)
                 point)))

      (define (normal-to-world normal)
        (let ((n (m4* (m4-transpose inv-transform) normal)))
          (tuple-set-w! n 0)
          (set! n (normalize n))
          (if parent
              (parent 'normal-to-world n)
              n)))

      (define (update-aabb!)
        (define (merge-child-boxes shapes result)
          (if (null? shapes)
              result
              (merge-child-boxes (cdr shapes)
                                 (aabb-merge ((car shapes) 'update-aabb!)
                                             result))))
        (let ((box (aabb-transform (merge-child-boxes shapes (empty-aabb))
                                   transform)))
          (set! aabb box)
          box))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args)))
              ;((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'world-to-object) (world-to-object (car args)))
              ((eq? m 'normal-to-world) (normal-to-world (car args)))
              ((eq? m 'transform) transform)
              ((eq? m 'local-intersect) (local-intersect (car args)))
              ((eq? m 'contains) (memq (car args) shapes))
              ((eq? m 'parent) parent)
              ((eq? m 'set-transform!)
               (set! transform (car args))
               (set! inv-transform (m4-inverse (car args))))
              ((eq? m 'shapes) shapes)
              ((eq? m 'add-children!) (add-children args))
              ((eq? m 'set-parent!) (set! parent (car args)))
              ((eq? m 'aabb) aabb)
              ((eq? m 'update-aabb!) (update-aabb!))
              (else (error "unknown method (group m ...)" m))))
      dispatch)

    (define (sphere-geometry)
      (define (intersect shape ray2)
        (let* ((sphere-to-ray (tuple-sub (ray-origin ray2)
                                         (point 0 0 0)))
               (dir (ray-direction ray2))
               (a (dot dir dir))
               (b (* 2 (dot dir
                            sphere-to-ray)))
               (c (- (dot sphere-to-ray
                          sphere-to-ray)
                     1))
               (discriminant (- (* b b)
                                (* 4 a c))))
          (if (< discriminant 0)
              '()
              (intersections
                (intersection
                  (/ (+ b (sqrt discriminant))
                     (* -2 a))
                  shape)
                (intersection
                  (/ (- b (sqrt discriminant))
                     (* -2 a))
                  shape)))))

      (define (normal-at obj-p)
        (tuple-sub obj-p (point 0 0 0)))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'contains) #f)
              ((eq? m 'aabb) (make-aabb -1 1 -1 1 -1 1))
              (else (error "unknown method (sphere-geometry m ...)" m))))

      dispatch)

    (define (plane-geometry)
      (define (intersect shape local-ray)
        (if (almost= 0 (tuple-y (ray-direction local-ray)))
            (intersections)
            (intersections
              (intersection (/ (- (tuple-y (ray-origin local-ray)))
                               (tuple-y (ray-direction local-ray)))
                            shape))))

      (define (normal-at local-p)
        (vec 0 1 0))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'contains) #f)
              ((eq? m 'aabb) (make-aabb -inf.0 +inf.0 (- EPSILON) EPSILON -inf.0 +inf.0))
              (else (error "unknown method (plane-geometry m ...)" m))))

      dispatch)

    (define (cube-geometry)
      (define (intersect shape local-ray)
        (let* ((origin (ray-origin local-ray))
               (direction (ray-direction local-ray))
               (x (check-axis (tuple-x origin) (tuple-x direction)))
               (y (check-axis (tuple-y origin) (tuple-y direction)))
               (z (check-axis (tuple-z origin) (tuple-z direction)))
               (t-min (max (car x) (car y) (car z)))
               (t-max (min (cdr x) (cdr y) (cdr z))))
          (if (< t-max t-min)
              (intersections)
              (intersections
                (intersection t-min shape)
                (intersection t-max shape)))))

      (define (normal-at local-p)
        (let ((x (tuple-x local-p))
              (y (tuple-y local-p))
              (z (tuple-z local-p)))
          (let ((maxc (max (abs x) (abs y) (abs z))))
            (cond ((= maxc (abs x)) (vec x 0 0))
                  ((= maxc (abs y)) (vec 0 y 0))
                  (else (vec 0 0 z))))))

      (define (check-axis origin direction)
        (if (< (abs direction) EPSILON)
            (cons (* (- -1 origin) +inf.0)
                  (* (- 1 origin) +inf.0))
            (cons-sort (/ (- -1 origin) direction)
                       (/ (- 1 origin) direction))))

      (define (cons-sort a b)
        (if (< b a)
            (cons b a)
            (cons a b)))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'contains) #f)
              ((eq? m 'aabb) (make-aabb -1 1 -1 1 -1 1))
              (else (error "unknown method (plane-geometry m ...)" m))))

      dispatch)

    (define (cylinder-geometry)
      (define minimum -inf.0)
      (define maximum +inf.0)
      (define closed? #f)

      (define (intersect shape ray)
        (let* ((dir (ray-direction ray))
               (a (+ (square (tuple-x dir))
                     (square (tuple-z dir)))))
          (if (< (abs a) EPSILON)
              (intersect-caps shape ray (intersections))
              (let* ((pos (ray-origin ray))
                     (b (+ (* (tuple-x pos)
                              (tuple-x dir)
                              2)
                           (* (tuple-z pos)
                              (tuple-z dir)
                              2)))
                     (c (+ (square (tuple-x pos))
                           (square (tuple-z pos))
                           -1))
                     (discriminant (- (* b b)
                                      (* 4 a c))))
                (if (< discriminant 0)
                    (intersections)
                    (intersect-caps shape ray
                      (let* ((t0 (/ (+ b (sqrt discriminant))
                                    (* -2 a)))
                             (t1 (/ (- b (sqrt discriminant))
                                    (* -2 a)))
                             (y0 (+ (tuple-y pos)
                                    (* t0 (tuple-y dir))))
                             (y1 (+ (tuple-y pos)
                                    (* t1 (tuple-y dir))))
                             (i1 (if (and (< minimum y1)
                                          (< y1 maximum))
                                     (intersections (intersection t1 shape))
                                     (intersections))))
                        (if (and (< minimum y0)
                                 (< y0 maximum))
                            (cons (intersection t0 shape)
                                  i1)
                            i1))))))))

      (define (intersect-caps shape ray xs)
        (if (and closed?
                 (< EPSILON (abs (tuple-y (ray-direction ray)))))
            (let ((t-min (/ (- minimum (tuple-y (ray-origin ray)))
                            (tuple-y (ray-direction ray))))
                  (t-max (/ (- maximum (tuple-y (ray-origin ray)))
                            (tuple-y (ray-direction ray)))))
              (if (cap? ray t-min)
                  (set! xs (merge-intersections
                             xs (intersections (intersection t-min shape)))))
              (if (cap? ray t-max)
                  (set! xs (merge-intersections
                             xs (intersections (intersection t-max shape)))))))
        xs)

      (define (cap? ray t)
        (let ((x (+ (tuple-x (ray-origin ray))
                    (* t (tuple-x (ray-direction ray)))))
              (z (+ (tuple-z (ray-origin ray))
                    (* t (tuple-z (ray-direction ray))))))
          (<= (+ (square x) (square z))
              1.0)))

      (define (normal-at obj-p)
        (let* ((dist (+ (square (tuple-x obj-p))
                        (square (tuple-z obj-p)))))
          (cond ((and (< dist 1.0)
                      (>= (tuple-y obj-p) (- maximum EPSILON)))
                 (vec 0 1 0))
                ((and (< dist 1.0)
                      (<= (tuple-y obj-p) (+ minimum EPSILON)))
                 (vec 0 -1 0))
                (else
                 (vec (tuple-x obj-p)
                      0
                      (tuple-z obj-p))))))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'contains) #f)
              ((eq? m 'minimum) minimum)
              ((eq? m 'maximum) maximum)
              ((eq? m 'closed?) closed?)
              ((eq? m 'set-minimum!) (set! minimum (car args)))
              ((eq? m 'set-maximum!) (set! maximum (car args)))
              ((eq? m 'set-closed!) (set! closed? (if (car args) #t #f)))
              ((eq? m 'aabb) (make-aabb -1 1 minimum maximum -1 1))
              (else (error "unknown method (cylinder-geometry m ...)" m))))

      dispatch)

    (define (cone-geometry)
      (define minimum -inf.0)
      (define maximum +inf.0)
      (define closed? #f)

      (define (intersect shape ray)
        (let* ((dir (ray-direction ray))
               (pos (ray-origin ray))
               (a (- (+ (square (tuple-x dir))
                        (square (tuple-z dir)))
                     (square (tuple-y dir))))
               (b (- (+ (* (tuple-x pos)
                           (tuple-x dir)
                           2)
                        (* (tuple-z pos)
                           (tuple-z dir)
                           2))
                     (* (tuple-y pos)
                        (tuple-y dir)
                        2)))
               (c (- (+ (square (tuple-x pos))
                        (square (tuple-z pos)))
                     (square (tuple-y pos)))))
          (if (< (abs a) EPSILON)
              (if (< (abs b) EPSILON)
                  (intersect-caps shape ray (intersections))
                  (intersect-caps shape ray
                                  (intersections
                                    (intersection (/ c -2 b) shape))))

              (let* ((discriminant (- (* b b)
                                      (* 4 a c))))
                (if (< discriminant 0)
                    (intersections)
                    (intersect-caps shape ray
                      (let* ((t0 (/ (+ b (sqrt discriminant))
                                    (* -2 a)))
                             (t1 (/ (- b (sqrt discriminant))
                                    (* -2 a)))
                             (y0 (+ (tuple-y pos)
                                    (* t0 (tuple-y dir))))
                             (y1 (+ (tuple-y pos)
                                    (* t1 (tuple-y dir))))
                             (i1 (if (and (< minimum y1)
                                          (< y1 maximum))
                                     (intersections (intersection t1 shape))
                                     (intersections))))
                        (if (and (< minimum y0)
                                 (< y0 maximum))
                            (cons (intersection t0 shape)
                                  i1)
                            i1))))))))

      (define (intersect-caps shape ray xs)
        (if (and closed?
                 (< EPSILON (abs (tuple-y (ray-direction ray)))))
            (let ((t-min (/ (- minimum (tuple-y (ray-origin ray)))
                            (tuple-y (ray-direction ray))))
                  (t-max (/ (- maximum (tuple-y (ray-origin ray)))
                            (tuple-y (ray-direction ray)))))
              (if (cap? ray t-min minimum)
                  (set! xs (merge-intersections
                             xs (intersections (intersection t-min shape)))))
              (if (cap? ray t-max maximum)
                  (set! xs (merge-intersections
                             xs (intersections (intersection t-max shape)))))))
        xs)

      (define (cap? ray t radius)
        (let ((x (+ (tuple-x (ray-origin ray))
                    (* t (tuple-x (ray-direction ray)))))
              (z (+ (tuple-z (ray-origin ray))
                    (* t (tuple-z (ray-direction ray))))))
          (<= (+ (square x) (square z))
              (square radius))))

      (define (normal-at obj-p)
        (let* ((dist (+ (square (tuple-x obj-p))
                        (square (tuple-z obj-p)))))
          (cond ((and (< dist 1.0)
                      (>= (tuple-y obj-p) (- maximum EPSILON)))
                 (vec 0 1 0))
                ((and (< dist 1.0)
                      (<= (tuple-y obj-p) (+ minimum EPSILON)))
                 (vec 0 -1 0))
                (else
                 (vec (tuple-x obj-p)
                      (if (< (tuple-y obj-p) 0)
                          (sqrt dist)
                          (- (sqrt dist)))
                      (tuple-z obj-p))))))

      (define (dispatch m . args)
        (cond ((eq? m 'intersect) (intersect (car args) (cadr args)))
              ((eq? m 'normal-at) (normal-at (car args)))
              ((eq? m 'contains) #f)
              ((eq? m 'minimum) minimum)
              ((eq? m 'maximum) maximum)
              ((eq? m 'closed?) closed?)
              ((eq? m 'set-minimum!) (set! minimum (car args)))
              ((eq? m 'set-maximum!) (set! maximum (car args)))
              ((eq? m 'set-closed!) (set! closed? (if (car args) #t #f)))
              ((eq? m 'aabb) (let ((radius (max (abs minimum) (abs maximum))))
                               (make-aabb (- radius) radius minimum maximum (- radius) radius)))
              (else (error "unknown method (cone-geometry m ...)" m))))

      dispatch)))
