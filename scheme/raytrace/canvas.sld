(define-library (raytrace canvas)
  (export canvas canvas-width canvas-height canvas-data canvas-clear! canvas->ppm
          pixel-set! pixel-get
          flat-index)
  (import (scheme base)
          (scheme write)
          (raytrace generic)
          (raytrace compare)
          (raytrace tuple))
  (begin
    (define-record-type <canvas>
      (make-canvas w h data)
      canvas?
      (w canvas-width)
      (h canvas-height)
      (data canvas-data canvas-set-data!))

    (define (canvas w h)
      (make-canvas w h (make-vector (* w h) (color 0 0 0))))

    (define (canvas-clear! canvas color)
      (vector-fill! (canvas-data canvas) color)
      canvas)

    (define (canvas-almost-equal? a b)
      (and (eq? (canvas-width a) (canvas-width b))
           (eq? (canvas-height a) (canvas-height b))
           (let ((alleq #t))
             (vector-for-each
               (lambda (x y) (set! alleq (and (almost-equal? x y) alleq)))
               (canvas-data a)
               (canvas-data b))
             alleq)))

    (define (canvas-print obj)
      (display "<canvas ")
      (display (canvas-width obj))
      (display " x ")
      (display (canvas-height obj))
      (display ">"))

    (define (canvas-dispatch method . args)
      (cond ((eq? 'print method) (apply canvas-print args))
            ((eq? 'almost-equal? method) (apply canvas-almost-equal? args))))

    (register-type canvas? canvas-dispatch)

    (define (pixel-set! canvas x y color)
      (vector-set! (canvas-data canvas)
                   (flat-index canvas x y)
                   color))

    (define (pixel-get canvas x y)
      (vector-ref (canvas-data canvas)
                  (flat-index canvas x y)))

    (define (flat-index canvas x y)
      (+ x (* y (canvas-width canvas))))

    (define (canvas->ppm canvas)
      (apply string-append
             (canvas->ppm-header canvas)
             (canvas->ppm-data canvas)))

    (define (canvas->ppm-header canvas)
      (string-append
        "P3\n"
        (number->string (canvas-width canvas))
        " "
        (number->string (canvas-height canvas))
        "\n"
        "255\n"))

    (define (canvas->ppm-data canvas)
      (let* ((data (canvas-data canvas))
             (n (vector-length data))
             (w (canvas-width canvas))
             (out '())
             (current-line ""))
        (let loop ((i 0))
          (if (= i n)
              (reverse (adjoin-line current-line out))
              (let ((c (color->string (vector-ref data i))))
                (set! current-line (string-append current-line c))
                (if (= (remainder i w) (- w 1))
                    (begin
                      (set! out (adjoin-line (string-append current-line "\n") out))
                      (set! current-line ""))
                    (set! current-line (string-append current-line " ")))
                (loop (+ i 1)))))))

    (define (adjoin-line line lines)
      (define (find-split line i)
        (if (equal? (string-ref line i) #\space)
            i
            (find-split line (- i 1))))
      (if (< (string-length line) 70)
          (cons line lines)
          (let ((split (find-split line 70)))
            (string-set! line split #\newline)
            (cons line lines))))

    (define (color->string c)
      (let ((c (color-round
                 (color-clip 0 255
                   (color-scale c 255)))))
        (string-append
            (number->string (color-red c))
            " "
            (number->string (color-green c))
            " "
            (number->string (color-blue c)))))))
