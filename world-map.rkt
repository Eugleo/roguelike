#lang racket

(require "tile.rkt")

(provide world-map set-wall! is-tile-walk-through? generate-world-map world-map-width world-map-height)

(struct world-map (width height tiles))

;; Generate (well, now it's hardcoded, but...) the whole terrain 
(define (generate-world-map width height)
  (define w-map (make-world-map width height))
  (set-wall! '((6 3) (7 3) (8 3) (8 4) (8 5)) w-map)
  w-map)

(define (make-world-map width height)
  (define tiles 
    (for/vector #:length width ([xi (in-range width)])
      (for/vector #:length height ([yi (in-range height)])
        (new tile% [see-through #t] [walk-through #t]))))
  (world-map width height tiles))

(define (set-tile! tile x y tiles)
  (vector-set! (vector-ref tiles x) y tile))

(define (get-tile x y w-map)
  (define tiles (world-map-tiles w-map))
  (if (and (< -1 x (vector-length tiles)) 
           (< -1 y (vector-length (vector-ref tiles 0))))
      (vector-ref (vector-ref tiles x) y)
      (print "Setting a tile failed")))

;; Can the tile at the specified coord be walked through?
(define (is-tile-walk-through? x y w-map)
  (send (get-tile x y w-map) is-walk-through?))

;; [(int, int) ...] world-map -> void 
;; Construct wall tiles on the given coords on the given world-map
(define (set-wall! coords w-map)
  (define tiles (world-map-tiles w-map))
  (for ([coord (in-list coords)])
    (define x (first coord))
    (define y (second coord))
    (set-tile! (new tile% [see-through #f] [walk-through #f]) x y tiles)))

