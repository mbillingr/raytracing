(import (scheme base)
        (scheme write)
        (raytrace testing)
        (raytrace tuple)
        (raytrace canvas))

(test "Creating a canvas"
  (given (c <- (canvas 10 20)))
  (then ((canvas-width c) == 10)
        ((canvas-height c) == 20)
        (c == (canvas-clear! (canvas 10 20) (color 0 0 0)))))

(test "Writing pixels to a canvas"
  (given (c <- (canvas 10 20))
         (red <- (color 1 0 0)))
  (when (pixel-set! c 2 3 red))
  (then ((pixel-get c 2 3) == red)))

(test "Constructing the PPM header and pixel data"
  (given (c <- (canvas 5 3))
         (c1 <- (color 1.5 0 0))
         (c2 <- (color 0 0.5 0))
         (c3 <- (color -0.5 0 1)))
  (when (pixel-set! c 0 0 c1)
        (pixel-set! c 2 1 c2)
        (pixel-set! c 4 2 c3)
        (ppm <- (canvas->ppm c)))
  (then (ppm == "P3
5 3
255
255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
")))

(test "Splitting long lines in PPM files"
  (given (c <- (canvas 10 2)))
  (when (canvas-clear! c (color 1 0.8 0.6))
        (ppm <- (canvas->ppm c)))
  (then (ppm == "P3
10 2
255
255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
153 255 204 153 255 204 153 255 204 153 255 204 153
255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
153 255 204 153 255 204 153 255 204 153 255 204 153
")))

(test "PPM files are terminated by a newline character"
  (given (c <- (canvas 5 3)))
  (when (ppm <- (canvas->ppm c)))
  (then ((string-ref ppm (- (string-length ppm) 1)) == #\newline)))
