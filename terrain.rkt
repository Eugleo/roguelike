#lang racket

;; The basic matrix holding the terrain info

(provide make-terrain terrain-width terrain-height terrain-is-place-walk-through? get-tile terrain-tiles tiles-width tiles-height)

(require "tile.rkt" "rectangle.rkt" "generator.rkt")

;; Datastrusture holding the static parts of the world (such as lakes, trees, grass)
(struct terrain (tiles))

;; int int -> terrain
;; Generate a terrain, given its dimensions
(define (make-terrain width height)
  (define gen (new village-generator% [width width] [height height]))
  (terrain (send gen generate-tiles width height)))

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
  (send (get-tile x y (terrain-tiles terrain)) walkable?))

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
      #f))

;; (val -> bool) (list val) -> bool
;; Reurn true iff all the values in a list satisfy a predicate
(define (all predicate lst) 
  (for/and ([item (in-list lst)]) (predicate item)))

;; int (list val) -> (list val)
;; Rotate the list by n (e.g. (rotate 2 (list 1 2 3 4)) is equal to (list 3 4 1 2))
(define (rotate n lst)
  (take (drop (append lst lst) (modulo (- (length lst) n) (length lst))) (length lst)))

