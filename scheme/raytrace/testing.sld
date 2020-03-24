(define-library (raytrace testing)
  (export test)
  (import (scheme base)
          (scheme write)
          (raytrace generic))
  (begin
    (define-syntax test
      (syntax-rules (given then <-)
        ((test description
           (given (var <- val) ...)
           (then (a == b) ...))
         (test description
           (given (var <- val) ...)
           (when)
           (then (a == b) ...)))

        ((test description
           (given (var <- val) ...)
           (when (var2 <- val2) ...)
           (then (a == b) ...))
         (let ((var val) ...)
           (let ((var2 val2) ...)
             (let ((a-val a)
                   (b-val b))
               (if (almost-equal? a-val b-val)
                   'pass
                   (begin
                     (display "FAIL: ")
                     (display description)
                     (newline)
                     (display "    ")
                     (display 'a)
                     (display " == ")
                     (display 'b)
                     (newline)
                     (display "    ")
                     (print a-val)
                     (display " == ")
                     (print b-val)
                     (newline)
                     (error description `(almost-equal? a b)))))
             ...
             (display "PASS: ")
             (display description)
             (newline))))))))
