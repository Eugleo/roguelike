#lang racket

;; The basic matrix holding the terrain info

(provide make-terrain terrain-width terrain-height terrain-is-place-walk-through?)

(require "tile.rkt")

;; Datastrusture holding the static parts of the world (such as lakes, trees, grass)
(struct terrain (tiles))

;; int int -> terrain
;; "Generate" a terrain, given its dimensions
(define (make-terrain width height)
  (define tiles 
    (for/vector #:length width ([xi (in-range width)])
      (for/vector #:length height ([yi (in-range height)])
        (new tile% [see-through #t] [walk-through #t]))))
  (terrain (tiles-add-walls '((6 3) (7 3) (8 3) (8 4) (8 5)) tiles)))

;; tile int int (matrixof tile) -> (matrixof tile)
;; Set the tile on the coords to a new tile
(define (tiles-set-tile tile x y tiles)
  (vector-set! (vector-ref tiles x) y tile)
  tiles)

;; int int terrain -> tile
;; Return the tile on the given coords
(define (terrain-get-tile x y terrain)
  (define tiles (terrain-tiles terrain))
  (if (and (< -1 x (vector-length tiles)) 
           (< -1 y (vector-length (vector-ref tiles 0))))
      (vector-ref (vector-ref tiles x) y)
      (print "Getting a tile failed")))

;; (listof (int, int)) (matrixof tile) -> void 
;; Construct wall tiles on coords
(define (tiles-add-walls coords tiles)
  (for/fold ([tiles tiles])
            ([coord (in-list coords)])
    (define x (first coord))
    (define y (second coord))
    (tiles-set-tile (new tile% [see-through #f] [walk-through #f]) x y tiles)))

;; terrain -> int 
;; Return the width (number of columns) of the terrain
(define (terrain-width terrain)
  (vector-length (terrain-tiles terrain)))

;; terrain -> int
;; Return the height (number of rows) of the terrain
(define (terrain-height terrain)
  (if (> (terrain-width terrain) 0)
      (vector-length (vector-ref (terrain-tiles terrain) 0))
      0))

;; int int terrain -> bool
;; Can the tile at the specified coord be walked through?
(define (terrain-is-place-walk-through? x y terrain)
  (send (terrain-get-tile x y terrain) is-walk-through?))
