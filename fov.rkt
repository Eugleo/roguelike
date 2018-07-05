#lang racket

(provide cast-light)

(require "terrain.rkt")

(define vis-radius 50)
(define vision-radius 20)

(define (cast-light x y tiles)
  (set-field! light (get-tile x y tiles) 5) 
  (make-scanner x y  0 1 1 0 tiles)
  (make-scanner x y 1 0 0 1 tiles) ; 1st octant
  (make-scanner x y 0 -1 1 0 tiles)
  (make-scanner x y -1 0 0 1 tiles)
  (make-scanner x y -1 0 0 -1 tiles)
  (make-scanner x y 0 -1 -1 0 tiles)
  (make-scanner x y 0 1 -1 0 tiles)
  (make-scanner x y 1 0 0 -1 tiles)) 
  
(define (make-scanner x y xx xy yx yy tiles)
  (define width (tiles-width tiles))
  (define height (tiles-height tiles))

  (define (run-scan dx row start-slope end-slope previous-blocked tiles)
    (define c-x (+ x (* xx dx) (* xy row)))
    (define c-y (+ y (* yx dx) (* yy row)))
    (define left-slope (/ (- dx 0.5) (+ row 0.5)))
    (define right-slope (/ (+ dx 0.5) (- row 0.5)))
    (define cell (get-tile c-x c-y tiles))
    (define should-end (or (> dx 0) (> end-slope left-slope)))
    (define next-row (sub1 row))
    (define next-cell (add1 dx))
    (cond
      [(< start-slope end-slope) tiles]
      [(> (- row) vision-radius) tiles] 
      [should-end (run-scan next-row next-row start-slope end-slope #t tiles)]
      [(or (not (and (>= c-x 0) (>= c-y 0) (< c-x width) (< c-y height)))) 
       (run-scan next-cell row start-slope end-slope previous-blocked tiles)] 
      [(< start-slope right-slope) 
       (run-scan next-cell row start-slope end-slope previous-blocked tiles)]
      [else
        (define current-blocked (not (send cell transparent?)))
        (cond [(<= (distance dx row) vision-radius) (set-field! light cell 5)])
        (cond
          [(and previous-blocked current-blocked)
           (run-scan next-cell row (min right-slope start-slope) end-slope #t tiles)]
          [current-blocked
           (define tiles/rec (run-scan next-row next-row start-slope (max end-slope left-slope) #f tiles))
           (run-scan next-cell row (min right-slope start-slope) end-slope current-blocked tiles)] 
          [else
           (run-scan next-cell row start-slope end-slope #f tiles)])]))

  (run-scan -1 -1 1 0 #f tiles))

(define (make-scanner2 x y start-slope end-slope start-row xx xy yx yy tiles)
  (define width (tiles-width tiles))
  (define height (tiles-height tiles))

  (define (scan-line previous-blocked distance strt new-start)
    (cond
      [(and (<= distance vis-radius) (not previous-blocked)) 
       (scan-cell #f (- distance) (- distance) new-start strt distance)]
      [else #f]))

  (define (scan-cell previous-blocked dx dy new-start strt distance)
    (define c-x (+ x (* xx dx) (* xy dy)))
    (define c-y (+ y (* yx dx) (* yy dy)))
    (define left-slope (/ (- dx 0.5) (+ dy 0.5)))
    (define right-slope (/ (+ dx 0.5) (- dy 0.5)))
    (define c-tile (get-tile c-x c-y tiles))
    
    (cond
      ;; We reached the border of the octant, continue scanning on the next row
      [(or (> dx 0)) (scan-line previous-blocked (add1 distance) strt new-start)]
      ;; We reached the border of the canvas, continue scanning on the next cell
      [(not (and (>= c-x 0) (>= c-y 0) (< c-x width) (< c-y height)))
       (scan-cell previous-blocked (add1 dx) dy new-start strt distance)]
      ;; We are to the left of the start slope, continue scanning the next cell
      [(< strt right-slope) (scan-cell previous-blocked (add1 dx) dy new-start strt distance)]
      ;; We are to the right of the end slope, continue scanning on the next row
      [(> end-slope left-slope) (scan-line previous-blocked (add1 distance) strt new-start)]
      ;; We are on the right spot, determine the lighting
      [else 
        ;; The distance from player is within vision radius, light the cell
        (cond [(<= (get-radius dx dy) vis-radius) (set-field! light c-tile 5)])

        (define current-blocked (not (send c-tile transparent?)))

        (cond
          [(and previous-blocked current-blocked)
           ;; Set new-start to the current right-slope 
           (scan-cell #t (add1 dx) dy right-slope strt distance)] 
          [previous-blocked 
            ;; Next scan starts from the new-start (that is, the right slope of the previous cell) 
            (scan-cell #f (add1 dx) dy new-start new-start distance)]
          [(and (< distance vis-radius) current-blocked)
           ;; Start a new scan with the same start slope, but with end slope before this cell
           (make-scanner x y strt left-slope (add1 (abs dy)) xx xy yx yy tiles)
           ;; Continue with the current scan on the next cell
           (scan-cell #t (add1 dx) dy right-slope strt distance)]
          [else
            ;; A normal unblocked cell, just continue with the scan
            (scan-cell #f (add1 dx) dy new-start strt distance)])]))

  (cond
    [(< start-slope end-slope) void]
    [else (scan-line #f start-row start-slope 0)]))

(define (get-radius x y)
  (sqrt (+ (sqr x) (sqr y))))

(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))


