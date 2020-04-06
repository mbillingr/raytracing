(import (scheme base)
        (raytrace testing)
        (raytrace world)
        (raytrace tuple)
        (raytrace transformations)
        (raytrace lights)
        (raytrace shapes)
        (raytrace material)
        (raytrace ray))

(test "Creating a world"
  (given (w <- (empty-world)))
  (then ((w 'objects) == '())
        ((w 'lights) == '())))

(test "The default world"
  (given (s1 <- (sphere))
         (s2 <- (sphere))
         (w <- (default-world)))

  (with (light <- (car (w 'lights)))
        (s1 <- (car (w 'objects)))
        (m1 <- (s1 'material))
        (s2 <- (cadr (w 'objects)))
        (t2 <- (s2 'transform)))
  (then ((light 'position) == (point -10 10 -10))
        ((light 'intensity) == (color 1 1 1))
        (t2 == (scaling 0.5 0.5 0.5))
        ((material-color m1) == (color 0.8 1.0 0.6))
        ((material-diffuse m1) == 0.7)
        ((material-specular m1) == 0.2)))

(test "Intersect a world with a ray"
  (given (w <- (default-world))
         (r <- (ray (point 0 0 -5) (vec 0 0 1))))
  (when (xs <- (w 'intersect r)))
  (then ((length xs) == 4)
        ((intersection-t (car xs)) == 4)
        ((intersection-t (cadr xs)) == 4.5)
        ((intersection-t (car (cddr xs))) == 5.5)
        ((intersection-t (cadr (cddr xs))) == 6)))

(test "Precomputing the state of an intersection"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (shape <- (sphere))
         (i <- (intersection 4 shape)))
  (when (c <- (prepare-computations i r)))
  (then ((comp-t c) == (intersection-t i))
        ((comp-object c) == (intersection-object i))
        ((comp-point c) == (point 0 0 -1))
        ((comp-eyev c) == (vec 0 0 -1))
        ((comp-normalv c) == (vec 0 0 -1))))

(test "The hit, when an intersection occurs on the outside"
  (given (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (shape <- (sphere))
         (i <- (intersection 4 shape)))
  (when (c <- (prepare-computations i r)))
  (then ((comp-inside? c) == #f)))

(test "The hit, when an intersection occurs on the inside"
  (given (r <- (ray (point 0 0 0) (vec 0 0 1)))
         (shape <- (sphere))
         (i <- (intersection 1 shape)))
  (when (c <- (prepare-computations i r)))
  (then ((comp-point c) == (point 0 0 1))
        ((comp-inside? c) == #t)
        ((comp-normalv c) == (vec 0 0 -1))))  ; inverted!

(test "Shading an intersection"
  (given (w <- (default-world))
         (r <- (ray (point 0 0 -5) (vec 0 0 1)))
         (shape <- (car (w 'objects)))
         (i <- (intersection 4 shape)))
  (when (comps <- (prepare-computations i r))
        (c <- (w 'shade-hit comps)))
  (then (c == (color 0.38066 0.47583 0.2855))))

(test "Shading an intersection from the inside"
  (given (w <- (default-world))
         (r <- (ray (point 0 0 0) (vec 0 0 1)))
         (shape <- (cadr (w 'objects)))
         (i <- (intersection 0.5 shape)))
  (when (w 'set-lights! (list (point-light (point 0 0.25 0) (color 1 1 1))))
        (comps <- (prepare-computations i r))
        (c <- (w 'shade-hit comps)))
  (then (c == (color 0.90498 0.90498 0.90498))))

(test "The color when a ray misses"
  (given (w <- (default-world))
         (r <- (ray (point 0 0 -5) (vec 0 1 0))))
  (when (c <- (w 'color-at r)))
  (then (c == (color 0 0 0))))

(test "The color when a ray hits"
  (given (w <- (default-world))
         (r <- (ray (point 0 0 -5) (vec 0 0 1))))
  (when (c <- (w 'color-at r)))
  (then (c == (color 0.38066 0.47583 0.2855))))

(test "The color with an intersection behind the ray"
  (given (w <- (default-world))
         (r <- (ray (point 0 0 0.75) (vec 0 0 -1))))
  (when (outer <- (car (w 'objects)))
        (material-set-ambient! (outer 'material) 1)
        (inner <- (cadr (w 'objects)))
        (material-set-ambient! (inner 'material) 1)
        (c <- (w 'color-at r)))
  (then (c == (material-color (inner 'material)))))

(test "There is no shadow if nothing is collinear with point and light"
  (given (w <- (default-world))
         (p <- (point 0 10 0)))
  (then ((w 'is-shadowed (car (w 'lights)) p) == #f)))

(test "There is shadow when an object is between point and light"
  (given (w <- (default-world))
         (p <- (point 10 -10 10)))
  (then ((w 'is-shadowed (car (w 'lights)) p) == #t)))

(test "There is no shadow if the object is behind the light"
  (given (w <- (default-world))
         (p <- (point -20 20 -20)))
  (then ((w 'is-shadowed (car (w 'lights)) p) == #f)))

(test "There is no shadow if the object is behind the point"
  (given (w <- (default-world))
         (p <- (point -2 2 -2)))
  (then ((w 'is-shadowed (car (w 'lights)) p) == #f)))
