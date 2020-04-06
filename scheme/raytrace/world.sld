(define-library (raytrace world)
  (export empty-world default-world make-world
          prepare-computations
          comp-t comp-object comp-point comp-eyev comp-normalv comp-inside?)
  (import (scheme base)
          (scheme inexact)
          (scheme write)
          (raytrace tuple)
          (raytrace ray)
          (raytrace matrix)
          (raytrace transformations)
          (raytrace material)
          (raytrace lights)
          (raytrace shapes))
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
      (define (intersect ray)
        (let loop ((objs objects))
          (if (null? objs)
              '()
              (merge-sorted (loop (cdr objs))
                            ((car objs) 'intersect ray)))))

      (define (shade-hit comps)
        (let loop ((l lights))
          (if (null? l)
              (color 0 0 0)
              (color+ (lighting ((comp-object comps) 'material)
                                (car l)
                                (comp-point comps)
                                (comp-eyev comps)
                                (comp-normalv comps))
                      (loop (cdr l))))))

      (define (color-at ray)
        (let ((i (hit (intersect ray))))
          (if i
              (let ((comps (prepare-computations i ray)))
                (shade-hit comps))
              (color 0 0 0))))

      (define (dispatch m . args)
        (cond ((eq? m 'color-at) (color-at (car args)))
              ((eq? m 'intersect) (intersect (car args)))
              ((eq? m 'shade-hit) (shade-hit (car args)))
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
             (inside (< (dot normv eyev) 0)))
        (make-comp t obj inside pos eyev
          (if inside (tuple-neg normv) normv))))

    (define-record-type <comp>
      (make-comp t object inside? point eyev normalv)
      comp?
      (t comp-t)
      (object comp-object)
      (inside? comp-inside?)
      (point comp-point)
      (eyev comp-eyev)
      (normalv comp-normalv))))
