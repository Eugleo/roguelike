#lang racket

(require "tile.rkt")

(provide (all-defined-out))

(struct world-map (width height tiles))

(define (make-world-map width height)
  (define tiles 
    (for/vector #:length width ([xi (in-range width)])
      (for/vector #:length height ([yi (in-range height)])
        (new tile% [see-through #t] [walk-through #t]))))
  (world-map width height tiles))

(define (set-tile! tile x y tiles)
  (vector-set!(vector-ref tiles x) y tile))

(define (get-tile x y w-map)
  (define tiles (world-map-tiles w-map))
  (if (and (< -1 x (vector-length tiles)) 
           (< -1 y (vector-length (vector-ref tiles 0))))
      (vector-ref (vector-ref tiles x) y)
      (print "Setting a tile failed")))

(define (is-tile-walk-through? x y w-map)
  (send (get-tile x y w-map) is-walk-through?))

(define (set-wall! coords w-map)
  (define tiles (world-map-tiles w-map))
  (for ([coord (in-list coords)])
    (define x (first coord))
    (define y (second coord))
    (set-tile! (new tile% [see-through #f] [walk-through #f]) x y tiles)))

