#lang racket

;; Basic, almost functional, impementation of recursive shadowcasting FOV algorithm

(provide cast-light)

(require "terrain.rkt")

(define vision-radius 20)

;; int int (matrix tiles) -> (matrix tiles)
;; Light up tiles which are in the FOV of the entity on x,y
;; Mutates the tiles, but returns the modified tiles as well
(define (cast-light x y tiles)
  ;; Holds a function which will light tiles laying in the FOV in all eight octants
  (define transformate
    (for/fold ([transformation values])
              ([xx (in-list '(0 1 0 -1 -1 0 0 1))]
               [xy (in-list '(1 0 -1 0 0 -1 1 0))]
               [yx (in-list '(1 0 1 0 0 -1 -1 0))]
               [yy (in-list '(0 1 0 1 -1 0 0 -1))])
      (compose1 (make-scanner x y xx xy yx yy tiles) transformation)))
  ;; The only place where tiles are mutated
  (transformate tiles)) 

;; int int int int int int (matrix tile) -> ((matrix tile) -> (matrix tile))
;; Return a function which will light up cells in FOV in the given octant once provided a matrix of cells
;; x, y specify the position of the player
;; xx, xy, yx, yy specify the transformations of the coordinates
(define (make-scanner x y xx xy yx yy tiles)
  (define width (tiles-width tiles))
  (define height (tiles-height tiles))
  ;; Run a scan on the given cell-index line-index coords
  ;; and then continue with the scan until the whole octant is scanned
  (define (run-scan cell-index line-index start-slope end-slope previous-blocked transformation)
    (define cell-x (- x (* xx cell-index) (* xy line-index)))
    (define cell-y (- y (* yx cell-index) (* yy line-index)))
    (define cell (get-tile cell-x cell-y tiles))
    (define in-bounds (and (>= cell-x 0) (>= cell-y 0) (< cell-x width) (< cell-y height)))
    (define next-line-index (add1 line-index))
    (define next-cell-index (add1 cell-index))
    ;; The slope of the cell which is closer to the stgiart-slope
    (define slope-s (/ (+ cell-index 0.5) (- line-index 0.5)))
    ;; The slope of the cell which is closer to the end-slope
    (define slope-e (/ (- cell-index 0.5) (+ line-index 0.5)))

    ;; We determine if we need to worry about lighting this cell at all
    ;; First three possibilities: no, the cell is in shadow and/or not visible
    (cond 
      ;; The scan is complete, there is nowhere to continue now
      [(or (> start-slope end-slope) ; Cell has inverted start and end slopes 
           (> line-index vision-radius)) ; Cell is beyond vision radius 
       transformation]
      [(> slope-e end-slope) ; Cell is in shadow (beyond the given end slope) 
       ;; Our job is done on the current line; start a new scan on the next line
       (run-scan 0 next-line-index start-slope end-slope #f transformation)]
      [(or (not in-bounds) ; Cell is not visible on the canvas
           (> start-slope slope-s)) ; Cell is in shadow (before the given start slope) 
       ;; Continue with scanning on this line (hoping to hit the start slope)
       (run-scan next-cell-index line-index start-slope end-slope previous-blocked transformation)] 
      [else ; The cell might be lighted, we need to find out
        (define current-blocked (not (send cell transparent?)))
        (define (new-transformation)
          (cond
            ;; If the cell is within vision radius, it is lighted
            [(<= (distance cell-index line-index) vision-radius)
             ;; That is, a function which lights the cell is added to the general transformation function
             (compose1 ((curry light-cell) cell-x cell-y) transformation)]
            [else transformation]))
        
        (cond ; Where do we continue with the scanning?
          [(and previous-blocked current-blocked)
           ;; We hit wall in a sequence of walls; set a new start slope and continue in this line
           (run-scan next-cell-index line-index slope-s end-slope #t (new-transformation))]
          [current-blocked
           ;; We hit a wall (either a solo wall or the first wall in a sequence)
           ;; Start a new scanner to scan the space in front of the wall in the subsequent lines
           (define trans/rec (run-scan 0 next-line-index start-slope slope-e #f (new-transformation)))
           ;; ...and then continue across the wall in the current line with a new start slope
           (run-scan next-cell-index line-index slope-s end-slope #t trans/rec)] 
          [else
           ;; We hit a plain transparent tile; continue with the scanning in this line
           (run-scan next-cell-index line-index start-slope end-slope #f (new-transformation))])]))

  ;; The initial call of the scanner
  (run-scan 0 0 ; The starting position of the scan is on the cell the player is standing on
            0 1 ; The start slope is 0 and the end slope is 1 (scan doesn't go beyond those values)
            #f ; The previous cell was not blocking light
            ((curry light-cell) x y))) ; The base function which lights up the cell player is standing on

;; int int (matrix tiles) -> (matrix tiles)
;; Helper function which lights up cell on the given coords and return the modified matrix of tiles
(define (light-cell x y cells)
  (set-field! light (get-tile x y cells) 5)
  cells)

;; int int -> float
;; Compute the (eucledian) distance of a point P[x, y] from point A[0, 0]
(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

