(define-library (raytrace lights)
  (export point-light)
  (import (scheme base)
          (raytrace tuple))
  (begin

    (define (point-light position intensity)

      (define (dispatch m . args)
        (cond ((eq? m 'position) position)
              ((eq? m 'intensity) intensity)
              (else (error "unknown method (point-light m ...)" m))))

      dispatch)))
