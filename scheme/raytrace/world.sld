(define-library (raytrace world)
  (export empty-world default-world make-world
          prepare-computations
          comp-t comp-object comp-point comp-over-point comp-eyev
          comp-normalv comp-inside? comp-reflectv)
  (import (scheme base)
          (scheme inexact)
          (scheme write)
          (raytrace tuple)
          (raytrace ray)
          (raytrace matrix)
          (raytrace transformations)
          (raytrace material)
          (raytrace lights)
          (raytrace shapes)
          (raytrace constants))
  (begin
    (define (empty-world)
      (make-world '() '()))

    (define (default-world)
      (let ((s1 (sphere))
            (s2 (sphere)))
        (s1 'set-material!
            (material (color 0.8 1.0 0.6)
                      0.1 0.7 0.2 200.0))
        (s2 'set-transform!
            (scaling 0.5 0.5 0.5))
        (make-world
          (list s1 s2)
          (list (point-light (point -10 10 -10) (color 1 1 1))))))

    (define (make-world objects lights)
      (define MAX-REFLECTION-DEPTH 3)
      (define (intersect ray)
        (let loop ((objs objects))
          (if (null? objs)
              '()
              (merge-sorted (loop (cdr objs))
                            ((car objs) 'intersect ray)))))

      (define (shade-hit comps remaining-bounces)
        (let loop ((l lights))
          (if (null? l)
              (color 0 0 0)
              (begin
                (color+
                  (color+
                    (lighting ((comp-object comps) 'material)
                              (comp-object comps)
                              (car l)
                              (comp-over-point comps)
                              (comp-eyev comps)
                              (comp-normalv comps)
                              (is-shadowed (car l) (comp-over-point comps)))
                    (reflected-color comps remaining-bounces))
                  (loop (cdr l)))))))

      (define (color-at ray remaining-bounces)
        (let ((i (hit (intersect ray))))
          (if i
              (let ((comps (prepare-computations i ray)))
                (shade-hit comps remaining-bounces))
              (color 0 0 0))))

      (define (is-shadowed light p)
        (define direction (tuple-sub (light 'position) p))
        (define h (hit (intersect (ray p (normalize direction)))))
        (and h (< (intersection-t h)
                  (magnitude direction))))

      (define (reflected-color comps remaining-bounces)
        (let ((r (material-reflective ((comp-object comps) 'material))))
          (if (or (= r 0) (= remaining-bounces 0))
              (color 0 0 0)
              (color-scale
                (color-at (ray (comp-over-point comps) (comp-reflectv comps))
                          (- remaining-bounces 1))
                r))))

      (define (dispatch m . args)
        (cond ((eq? m 'color-at) (color-at (car args) MAX-REFLECTION-DEPTH))
              ((eq? m 'intersect) (intersect (car args)))
              ((eq? m 'is-shadowed) (is-shadowed (car args) (cadr args)))
              ((eq? m 'shade-hit) (shade-hit (car args) MAX-REFLECTION-DEPTH))
              ((eq? m 'reflected-color) (reflected-color (car args) (cadr args)))
              ((eq? m 'lights) lights)
              ((eq? m 'objects) objects)
              ((eq? m 'add-object!) (set! objects (cons (car args) objects)))
              ((eq? m 'add-light!) (set! lights (cons (car args) lights)))
              ((eq? m 'set-objects!) (set! objects (car args)))
              ((eq? m 'set-lights!) (set! lights (car args)))
              (else (error "unknown method (world m ...)" m))))

      dispatch)

    (define (merge-sorted seq1 seq2)
      (cond ((null? seq1) seq2)
            ((null? seq2) seq1)
            ((< (intersection-t (car seq2))
                (intersection-t (car seq1)))
             (cons (car seq2)
                   (merge-sorted seq1
                                 (cdr seq2))))
            (else
             (cons (car seq1)
                   (merge-sorted (cdr seq1)
                                 seq2)))))

    (define (prepare-computations i ray)
      (let* ((t (intersection-t i))
             (obj (intersection-object i))
             (pos (ray-position ray t))
             (eyev (tuple-neg (ray-direction ray)))
             (normv (obj 'normal-at pos))
             (inside (< (dot normv eyev) 0))
             (normv (if inside (tuple-neg normv) normv))
             (over-pos (tuple-add pos (tuple-scale normv EPSILON)))
             (reflectv (reflect (ray-direction ray) normv)))
        (make-comp t obj inside pos over-pos eyev normv reflectv)))

    (define-record-type <comp>
      (make-comp t object inside? point over-point eyev normalv reflectv)
      comp?
      (t comp-t)
      (object comp-object)
      (inside? comp-inside?)
      (point comp-point)
      (over-point comp-over-point)
      (eyev comp-eyev)
      (normalv comp-normalv)
      (reflectv comp-reflectv))))
