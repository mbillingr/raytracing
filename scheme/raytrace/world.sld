(define-library (raytrace world)
  (export empty-world default-world make-world
          prepare-computations
          comp-t comp-object comp-point comp-over-point comp-under-point
          comp-eyev comp-normalv comp-inside? comp-reflectv comp-nu1 comp-nu2)
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

    (define-syntax prepare-computations
      (syntax-rules ()
        ((prepare-computations i ray xs) (prepare-computations* i ray xs))
        ((prepare-computations i ray) (prepare-computations* i ray `(,i)))))

    (define (make-world objects lights)
      (define MAX-BOUNCE-DEPTH 5)
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
                    (color+
                      (reflected-color comps remaining-bounces)
                      (refracted-color comps remaining-bounces)))
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

      (define (refracted-color comps remaining-bounces)
        (let ((t (material-transparency ((comp-object comps) 'material))))
          (if (or (= t 0) (= remaining-bounces 0))
              (color 0 0 0)
              (let* ((nu-ratio (/ (comp-nu1 comps) (comp-nu2 comps)))
                     (cos-i (dot (comp-eyev comps) (comp-normalv comps)))
                     (sin2-t (* nu-ratio nu-ratio (- 1.0 (* cos-i cos-i)))))
                (if (< 1 sin2-t)
                    (color 0 0 0)
                    (let* ((cos-t (sqrt (- 1.0 sin2-t)))
                           (direction (tuple-sub (tuple-scale (comp-normalv comps)
                                                              (- (* nu-ratio cos-i)
                                                                 cos-t))
                                                 (tuple-scale (comp-eyev comps)
                                                              nu-ratio)))
                           (refract-ray (ray (comp-under-point comps)
                                             direction)))
                      (color-scale
                        (color-at refract-ray (- remaining-bounces 1))
                        t)))))))

      (define (dispatch m . args)
        (cond ((eq? m 'color-at) (color-at (car args) MAX-BOUNCE-DEPTH))
              ((eq? m 'intersect) (intersect (car args)))
              ((eq? m 'is-shadowed) (is-shadowed (car args) (cadr args)))
              ((eq? m 'shade-hit) (shade-hit (car args) MAX-BOUNCE-DEPTH))
              ((eq? m 'reflected-color) (reflected-color (car args) (cadr args)))
              ((eq? m 'refracted-color) (refracted-color (car args) (cadr args)))
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

    (define (prepare-computations* i ray xs)
      (let* ((t (intersection-t i))
             (obj (intersection-object i))
             (pos (ray-position ray t))
             (eyev (tuple-neg (ray-direction ray)))
             (normv (obj 'normal-at pos))
             (inside (< (dot normv eyev) 0))
             (normv (if inside (tuple-neg normv) normv))
             (delta (tuple-scale normv EPSILON))
             (over-pos (tuple-add pos delta))
             (under-pos (tuple-sub pos delta))
             (reflectv (reflect (ray-direction ray) normv))
             (nus (compute-intersection-refractive-indices i xs))
             (nu1 (car nus))
             (nu2 (cdr nus)))
        (make-comp t obj inside pos over-pos under-pos
                   eyev normv reflectv nu1 nu2)))

    (define-record-type <comp>
      (make-comp t object inside? point over-point under-point
                 eyev normalv reflectv nu1 nu2)
      comp?
      (t comp-t)
      (object comp-object)
      (inside? comp-inside?)
      (point comp-point)
      (over-point comp-over-point)
      (under-point comp-under-point)
      (eyev comp-eyev)
      (normalv comp-normalv)
      (reflectv comp-reflectv)
      (nu1 comp-nu1)
      (nu2 comp-nu2))

    (define (compute-intersection-refractive-indices i xs)
      (let ((nu1 #f) (nu2 #f))
        (let loop ((xs xs)
                   (containers '()))
          (let ((post-containers (adjoin-or-remove containers (intersection-object (car xs)))))
            (if (eq? i (car xs))
                (cons (if (null? containers)
                          1
                          (material-refractive-index ((car containers) 'material)))
                      (if (null? post-containers)
                          1
                          (material-refractive-index ((car post-containers) 'material))))
                (begin
                  (loop (cdr xs) post-containers)))))))

    (define (adjoin-or-remove sequence obj)
      (define should-adjoin? #t)
      (define (loop seq)
        (cond ((null? seq) '())
              ((eq? (car seq) obj)
               (begin
                 (set! should-adjoin? #f)
                 (cdr seq)))
              (else (cons (car seq)
                          (loop (cdr seq))))))
      (let ((new-seq (loop sequence)))
        (if should-adjoin?
            (cons obj new-seq)
            new-seq)))))
