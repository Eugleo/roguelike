#lang racket

(require "tile.rkt")

(provide (all-defined-out))

(define world-map%
  (class object%
    
    (init-field width height)
    (define/public (get-width) width)
    (define/public (get-height) height)
    
    (define tiles 
      (for/vector #:length width ([xi (in-range width)])
        (for/vector #:length height ([yi (in-range height)])
          (new tile% [see-through #t] [walk-through #t]))))

    (define/public (get-tiles) tiles)

    (define (set-tile! tile x y)
      (if 
        (and (< -1 x (vector-length tiles)) 
             (< -1 y (vector-length (vector-ref tiles 0))))
        (vector-set! (vector-ref tiles x) y tile)
        (print "Setting a tile failed")))

    (define/public (set-wall! coords)
      (for ([coord (in-list coords)])
        (define x (first coord))
        (define y (second coord))
        (set-tile! (new tile% [see-through #f] [walk-through #f]) x y)))

    (define/public (get-tile x y)
      (if 
        (and (< -1 x (vector-length tiles)) 
             (< -1 y (vector-length (vector-ref tiles 0))))
        (vector-ref (vector-ref tiles x) y)
        (print "Setting a tile failed")))

    (define/public (is-tile-walk-through? x y)
      (send (get-tile x y) is-walk-through?))

    (super-new)))
