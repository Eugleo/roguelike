#lang racket

;; The world and associated functions

(provide world-player make-world world-current-region)

(require "entities.rkt" "region.rkt" "terrain.rkt")

;; Datastructure holding the entities of the game, as well as the whole environment
(struct world (regions player))

;; int int -> world
;; "Generate" a world, given the dimensions of its regions
(define (make-world width height)
  (define region (make-region width height)) ; Getting ready for generated content
  (define-values (x y) (random-walkable-tile-coords (region-terrain region)))
  (define player (new player% [x 8] [y 13]))
  (world (list region) player))

;; world -> region
;; Return the active (shown) region of the world
(define (world-current-region world)
  (first (world-regions world)))
