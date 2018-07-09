#lang racket

;; Recursive shadowcasting FOV algorithm
;; Implementation of: http://www.roguebasin.com/index.php?title=FOV_using_recursive_shadowcasting

(provide cast-light)

(require "terrain.rkt")

(define vision-radius 20)

;; int int (matrix cell) -> (matrix cell)
;; Compute and draw FOV of the entity at x, y without mutating the original matrix
(define (cast-light x y cells)
  (define cells-copy (vector-copy cells))
  ;; A composed transformation function of type ((matrix cell) ->  (matrix cell)) 
  ;; which composes the transfomration functions of all eight octants 
  (define transformation
    (for/fold ([trans values])
              ([xx (in-list '(0 1 0 -1 -1 0 0 1))]
               [xy (in-list '(1 0 -1 0 0 -1 1 0))]
               [yx (in-list '(1 0 1 0 0 -1 -1 0))]
               [yy (in-list '(0 1 0 1 -1 0 0 -1))])
     (compose1 (make-scanner x y xx xy yx yy cells) trans)))
  (transformation cells-copy))

;; int int int int int int (matrix cell) -> ((matrix cell) ->  (matrix cell))
;; Create a new scanner for an octant
;; x, y specify the position of the player
;; xx, xy, yx, yy specify the transformations of the coordinates
;; Returns a function which takes cells, draws FOV onto them and returns them
(define (make-scanner x y xx xy yx yy cells)
  (define width (tiles-width cells))
  (define height (tiles-height cells))
  (define (set-cell-to-lit loc-x loc-y cells)
    (set-field! light (get-tile loc-x loc-y cells) 5) 
    cells)

  ;; Run a scan on the given cell-index line-index coords
  ;; and then continue with the scan until the whole octant is scanned
  (define (run-scan cell-index line-index start-slope end-slope previous-blocked transformation)
    (define cell-x (- x (* xx cell-index) (* xy line-index)))
    (define cell-y (- y (* yx cell-index) (* yy line-index)))
    (define cell (get-tile cell-x cell-y cells))
    (define in-bounds (and (>= cell-x 0) (>= cell-y 0) (< cell-x width) (< cell-y height)))
    (define next-line-index (add1 line-index))
    (define next-cell-index (add1 cell-index))

    ;; slope-s is the slope of the cell which is closer to the start-slope
    ;; slope-e is the slope of the cell which is closer to the end-slope
    (define slope-s (/ (+ cell-index 0.5) (- line-index 0.5)))
    (define slope-e (/ (- cell-index 0.5) (+ line-index 0.5)))

    (define slope-c (/ cell-index line-index))

    ;; We determine if we need to worry about lighting this cell at all
    ;; First three possibilities: no, the cell is in shadow and/or not visible
    (cond 
      [(or (> start-slope end-slope) ; Cell has inverted start and end slopes 
           (> line-index vision-radius)) ; Cell is beyond vision radius 
       ;; End this scan completely; it has no point to scan further
       transformation]
      [(> slope-e end-slope) ; Cell is in shadow (beyond the given end slope) 
       ;; Our job is done on the current line; start a new scan on the next line
       (run-scan 0 next-line-index start-slope end-slope #f transformation)]
      [(or (not in-bounds) ; Cell is not visible on the canvas
           (> start-slope slope-s)) ; Cell is in shadow (before the given start slope) 
       ;; Continue with scanning on this line (hoping to hit the start slope)
       (run-scan next-cell-index line-index start-slope end-slope previous-blocked transformation)] 
      [else ; The cell might be lit, we need to find out
        (define current-blocked (not (send cell transparent?)))
        ;; If the cell is within vision radius, it is lit
        (define trans-result
          (cond
            ;; Add a "light this cell" function to the transformation
            [(<= (distance cell-index line-index) vision-radius) 
             (compose1 ((curry set-cell-to-lit) cell-x cell-y) transformation)]
            ;; The original transformation, the cell stays in shadow
            [else transformation]))

        (cond ; Where do we continue with the scanning?
          [(and previous-blocked current-blocked)
           ;; We hit wall in a sequence of walls; set a new start slope and continue in this line
           (run-scan next-cell-index line-index slope-s end-slope #t trans-result)]
          [current-blocked
           ;; We hit a wall (either a solo wall or the first wall in a sequence)
           ;; Start a new scanner to scan the space in front of the wall in the subsequent lines
           (define trans/rec (run-scan 0 next-line-index start-slope slope-e #f trans-result))
           ;; ...and then continue across the wall in the current line with a new start slope
           (run-scan next-cell-index line-index slope-s end-slope #t trans/rec)] 
          [else
           ;; We hit a plain transparent cell; continue with the scanning in this line
           (run-scan next-cell-index line-index start-slope end-slope #f trans-result)])]))

  ;; Run the scan with initial configuration
  (run-scan 1 1 ; Start on the first cell of the first line (next to the cell where player is) 
            0 1 ; Start with start-slope set to 0 and end-slope set to 1 (the bounds for a octant)
            #f ; Previous cell was not a wall 
            ((curry set-cell-to-lit) x y))) ; A function which sets the player's cell to lit 

;; int int -> float
;; Compute the (Eucleidian) distance of the point P[x,y] from the point A[0,0]
(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

