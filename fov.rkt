#lang racket

(provide cast-light)

(require "terrain.rkt")

(define vision-radius 20)

(define (cast-light x y tiles)
  (make-scanner x y 0 1 1 0 tiles)
  (make-scanner x y 1 0 0 1 tiles) ; 1st octant
  (make-scanner x y 0 -1 1 0 tiles)
  (make-scanner x y -1 0 0 1 tiles)
  (make-scanner x y -1 0 0 -1 tiles)
  (make-scanner x y 0 -1 -1 0 tiles)
  (make-scanner x y 0 1 -1 0 tiles)
  (make-scanner x y 1 0 0 -1 tiles)) 

;; int int int int int int (matrix tile) -> (matrix tile)
;; Create a new scanner for an octant
;; x, y specify the position of the player
;; xx, xy, yx, yy specify the transformations of the coordinates
(define (make-scanner x y xx xy yx yy tiles)
  (define width (tiles-width tiles))
  (define height (tiles-height tiles))

  ;; Run a scan on the given cell-index line-index coords
  ;; and then continue with the scan until the whole octant is scanned
  (define (run-scan cell-index line-index start-slope end-slope previous-blocked tiles)
    (define cell-x (- x (* xx cell-index) (* xy line-index)))
    (define cell-y (- y (* yx cell-index) (* yy line-index)))
    (define cell (get-tile cell-x cell-y tiles))
    (define in-bounds (and (>= cell-x 0) (>= cell-y 0) (< cell-x width) (< cell-y height)))
    (define next-line-index (add1 line-index))
    (define next-cell-index (add1 cell-index))

    ;; slope-s is the slope of the cell which is closer to the start-slope
    ;; slope-e is the slope of the cell which is closer to the end-slope
    (define slope-s (/ (+ cell-index 0.5) (- line-index 0.5)))
    (define slope-e (/ (- cell-index 0.5) (+ line-index 0.5)))

    ;; We determine if we need to worry about lighting this cell at all
    ;; First three possibilities: no, the cell is in shadow and/or not visible
    (cond 
      [(or (> start-slope end-slope) ; Cell has inverted start and end slopes 
           (> line-index vision-radius)) ; Cell is beyond vision radius 
       tiles]
      [(> slope-e end-slope) ; Cell is in shadow (beyond the given end slope) 
       ;; Our job is done on the current line; start a new scan on the next line
       (run-scan 0 next-line-index start-slope end-slope #f tiles)]
      [(or (not in-bounds) ; Cell is not visible on the canvas
           (> start-slope slope-s)) ; Cell is in shadow (before the given start slope) 
       ;; Continue with scanning on this line (hoping to hit the start slope)
       (run-scan next-cell-index line-index start-slope end-slope previous-blocked tiles)] 
      [else ; The cell might be lighted, we need to find out
        (define current-blocked (not (send cell transparent?)))
        ;; If the cell is within vision radius, it is lighted
        (cond [(<= (distance cell-index line-index) vision-radius) (set-field! light cell 5)])
        (cond ; Where do we continue with the scanning?
          [(and previous-blocked current-blocked)
           ;; We hit wall in a sequence of walls; set a new start slope and continue in this line
           (run-scan next-cell-index line-index slope-s end-slope #t tiles)]
          [current-blocked
           ;; We hit a wall (either a solo wall or the first wall in a sequence)
           ;; Start a new scanner to scan the space in front of the wall in the subsequent lines
           (define tiles/rec (run-scan 0 next-line-index start-slope slope-e #f tiles))
           ;; ...and then continue across the wall in the current line with a new start slope
           (run-scan next-cell-index line-index slope-s end-slope #t tiles)] 
          [else
           ;; We hit a plain transparent tile; continue with the scanning in this line
           (run-scan next-cell-index line-index start-slope end-slope #f tiles)])]))

  (run-scan 0 0 0 1 #f tiles))

(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

