(define-library (raytrace testing)
  (export test)
  (import (scheme base)
          (scheme write)
          (raytrace generic))
  (begin
    (define-syntax test
      (syntax-rules (given then <- == !=)
        ((test description
           (given (var <- val) ...)
           (then (a cmp b) ...))
         (test description
           (given (var <- val) ...)
           (when)
           (then (a cmp b) ...)))

        ((test description
           (given (var <- val) ...)
           (when items ...)
           (then (a cmp b) ...))
         (let* ((var val) ...)
           (test "when" items ...
             (let ((a-val a)
                   (b-val b))
               (if (test "compare" cmp a-val b-val)
                   'pass
                   (begin
                     (display "FAIL: ")
                     (display description)
                     (newline)
                     (display "    ")
                     (display 'a)
                     (display " ")
                     (display 'cmp)
                     (display " ")
                     (display 'b)
                     (newline)
                     (display "    ")
                     (print a-val)
                     (display " ")
                     (display 'cmp)
                     (display " ")
                     (print b-val)
                     (newline)
                     (error description `(almost-equal? a b)))))
             ...
             (display "PASS: ")
             (display description)
             (newline))))

        ((test "compare" == a b)
         (almost-equal? a b))

        ((test "compare" != a b)
         (not (almost-equal? a b)))

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
