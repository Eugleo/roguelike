#lang racket

(provide get-bitmap-for-tile get-bitmap-for-entity)

(require "tile.rkt" "entities.rkt")

(define (get-bitmap-for-tile tile)
  (define coords 
    (cond
    [(is-a? tile wall%)
                (case (get-field orientation tile)
                  [(horizontal) (hash-ref tile-bm-coords-hash 'wall-horizontal)]
                  [(vertical) (hash-ref tile-bm-coords-hash 'wall-vertical)]
                  [(up-right) (hash-ref tile-bm-coords-hash 'wall-top-right)]
                  [(up-left) (hash-ref tile-bm-coords-hash 'wall-top-left)]
                  [(down-right) (hash-ref tile-bm-coords-hash 'wall-bottom-right)]
                  [(down-left) (hash-ref tile-bm-coords-hash 'wall-bottom-left)]
                  [(cross) (hash-ref tile-bm-coords-hash 'wall-cross)]
                  [(t-up) (hash-ref tile-bm-coords-hash 'wall-t-top)]
                  [(t-right) (hash-ref tile-bm-coords-hash 'wall-t-right)]
                  [(t-down) (hash-ref tile-bm-coords-hash 'wall-t-bottom)]
                  [(t-left) (hash-ref tile-bm-coords-hash 'wall-t-left)])]
               [(is-a? tile wooden-floor%) (hash-ref tile-bm-coords-hash 'wooden-floor)]
               [(is-a? tile grass%) (hash-ref tile-bm-coords-hash 'grass)]
               [(is-a? tile door%) (hash-ref tile-bm-coords-hash 'door-open)]))
  (values (first coords) (second coords)))

(define tile-bm-coords-hash
  (make-immutable-hash
    '([wall-top-right . (5 0)]
     [wall-bottom-right . (0 0)]
     [wall-top-left . (4 0)]
     [wall-bottom-left . (1 0)]
     [wall-t-top . (1 1)]
     [wall-t-bottom . (0 1)]
     [wall-t-left . (2 1)]
     [wall-t-right . (3 1)]
     [wall-cross . (4 1)]
     [wall-horizontal . (2 0)]
     [wall-vertical . (3 0)]
     [grass . (7 0)]
     [door-open . (6 0)]
     [wooden-floor . (9 0)])))


(define (get-bitmap-for-entity entity)
  (define coords
    (cond
      [(is-a? entity player%) (hash-ref entity-bm-coords-hash 'player)]
      [(is-a? entity person%) (hash-ref entity-bm-coords-hash 'npc)]))
  (values (first coords) (second coords)))


(define entity-bm-coords-hash
  (make-immutable-hash
    '([player . (0 4)]
     [npc . (0 3)]
     [monster  . (13 4)])))
