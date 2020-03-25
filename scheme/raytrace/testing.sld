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
           (when items ...)
           (then (a == b) ...))
         (let ((var val) ...)
           (test "when" items ...
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
             (newline))))

        ((test "when" body)
         body)

        ((test "when" item body)
         (test "when-item" item body))

        ((test "when" item more ... body)
         (test "when-item" item (test "when" more ... body)))

        ((test "when-item" (var <- val) body)
         (let ((var val))
           body))

        ((test "when-item" item body)
         (begin
           item
           body))))))
