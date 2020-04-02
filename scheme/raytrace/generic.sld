(define-library (raytrace generic)
  (export register-type dispatch1 dispatch2
          almost-equal? print)
  (import (scheme base)
          (scheme write)
          (raytrace compare))
  (begin
    (define types '())

    (define (register-type predicate dispatch)
      (set! types `((,predicate . ,dispatch) . ,types)))

    (define (print obj)
      (dispatch1 'print display obj))

    (define (almost-equal? a b)
      (dispatch2
        'almost-equal?
        (lambda (a b)
          (cond ((and (number? a) (number? b))
                 (almost= a b))
                ((and (procedure? a) (procedure? b))
                 (eq? a b))
                ((and (pair? a) (pair? b))
                 (and (almost-equal? (car a) (car b))
                      (almost-equal? (cdr a) (cdr b))))
                (else (equal? a b))))
        a b))

    (define (dispatch1 method default obj . args)
      (define (loop types)
        (cond ((null? types) (apply default obj args))
              (((caar types) obj)
               (apply (cdar types) method obj args))
              (else (loop (cdr types)))))
      (loop types))

    (define (dispatch2 method default obj1 obj2 . args)
      (define (loop types)
        (cond ((null? types) (apply default obj1 obj2 args))
              ((and ((caar types) obj1)
                    ((caar types) obj2))
               (apply (cdar types) method obj1 obj2 args))
              (else (loop (cdr types)))))
      (loop types))))
