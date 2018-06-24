#lang racket

;; The basic matrix holding the terrain info

(provide make-terrain terrain-width terrain-height terrain-is-place-walk-through? terrain-rooms)

(require "tile.rkt" "rectangle.rkt")

;; Datastrusture holding the static parts of the world (such as lakes, trees, grass)
(struct terrain (rooms tiles))

;; int int -> terrain
;; "Generate" a terrain, given its dimensions
(define (make-terrain width height)
  (define rooms (generate-rooms 5 #:size-bounds (list 3 8) #:position-bounds (list width height)))
  (terrain rooms (generate-tiles rooms width height)))

;; terrain -> int 
;; Return the width (number of columns) of the terrain
(define (terrain-width terrain)
  (tiles-width (terrain-tiles terrain)))

;; terrain -> int
;; Return the height (number of rows) of the terrain
(define (terrain-height terrain)
  (tiles-height (terrain-tiles terrain)))

;; int int terrain -> bool
;; Can the tile at the specified coord be walked through?
(define (terrain-is-place-walk-through? x y terrain)
  (send (get-tile x y (terrain-tiles terrain)) is-walk-through?))


;; (matrix tile) -> int
;;; Return the width of the map
(define (tiles-width tiles)
  (vector-length tiles))

;; (matrix tile) -> int
;;; Return the height of the map
(define (tiles-height tiles)
  (if (> (tiles-width tiles) 0)
      (vector-length (vector-ref tiles 0))
      0))

;; int int terrain -> tile
;; Return the tile on the given coords
(define (get-tile x y tiles)
  (if (and (< -1 x (vector-length tiles)) 
           (< -1 y (vector-length (vector-ref tiles 0))))
      (vector-ref (vector-ref tiles x) y)
      (error "Getting a tile failed")))

;; (list rect) int int -> (matrix tile)
;; Return a matrix of tiles with rendered rooms, corridors and walls
(define (generate-tiles rooms width height)
  (define blank-tiles 
    (for/vector #:length width ([xi (in-range width)])
      (for/vector #:length height ([yi (in-range height)])
        (new tile% [see-through #f] [walk-through #f]))))
  (for/fold ([tiles blank-tiles]
             #:result (generate-corridors tiles rooms))
            ([room (in-list rooms)])
    (tiles-add-room room tiles)))

;; int (int, int) (int, int) -> (list rect)
;; Generate a given number of rooms of random size and position
(define (generate-rooms number #:size-bounds size-bounds #:position-bounds pos-bounds)
  (define min-size (first size-bounds))
  (define max-size (second size-bounds))
  (define max-x (first pos-bounds))
  (define max-y (second pos-bounds))
  (for/fold ([rooms '()])
            ([i (in-naturals)] #:break (= (length rooms) number))
    (define width (random min-size (add1 max-size)))
    (define height (random min-size (add1 max-size)))
    (define x (random 1 (- max-x width)))
    (define y (random 1 (- max-y height)))
    (define room (rect x y width height))
    (cond
      [(all ((curry (compose not rect-intersects?)) room) rooms) (cons room rooms)]
      [else rooms])))

;; (matrix tile) (list room) -> (matrix tile)
;; Return matrix of tiles with the rooms interconnected by a generated corridor
(define (generate-corridors tls rooms)
  (for/fold ([tiles tls])
            ([room1 rooms]
             [room2 (rotate 1 rooms)])
    (define-values (c1-x c1-y) (rect-center room1))
    (define-values (c2-x c2-y) (rect-center room2))
    (cond 
      [(eq? (random 2) 1)
        (create-vertical-corridor c1-y c2-y c2-x (create-horizontal-corridor c1-x c2-x c1-y tiles))]
      [else
        (create-horizontal-corridor c1-x c2-x c2-y (create-vertical-corridor c1-y c2-y c1-x tiles))])
    tiles))

;; rect (matrix tile) -> (matrix tile)
;; Add a room (= make certain tiles walk-through) to an existing matrix of tiles
(define (tiles-add-room rect tiles)
  (for* ([x (in-range (rect-x rect) (rect-x-bound rect))]
         [y (in-range (rect-y rect) (rect-y-bound rect))])
    (define tile (get-tile x y tiles))
    (set-field! walk-through tile #t))
  tiles)

;; int int int (matrix tile) -> (matrix tile)
;; Connect two points with a horizontal corridor
(define (create-horizontal-corridor from-x to-x y tiles)
  (for ([x (in-range (min from-x to-x) (add1 (max from-x to-x)))])
    (define tile (get-tile x y tiles))
    (set-field! walk-through tile #t))
  tiles)

;; int int int (matrix tile) -> (matrix tile)
;; Connect two points with a vertical corridor
(define (create-vertical-corridor from-y to-y x tiles)
  (for ([y (in-range (min from-y to-y) (add1 (max from-y to-y)))])
    (define tile (get-tile x y tiles))
    (set-field! walk-through tile #t))
  tiles)

;; (val -> bool) (list val) -> bool
;; Reurn true iff all the values in a list satisfy a predicate
(define (all predicate lst) (= (length lst) (count predicate lst)))

;; int (list val) -> (list val)
;; Rotate the list by n (e.g. (rotate 2 (list 1 2 3 4)) is equal to (list 3 4 1 2))
(define (rotate n lst)
  (take (drop (append lst lst) (modulo (- (length lst) n) (length lst))) (length lst)))

