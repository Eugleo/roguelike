#lang racket

;; A piece of world, should fit one screen

(provide make-region region-entities region-terrain random-walkable-tile-coords)

(require "tile.rkt" "terrain.rkt" "entities.rkt" "rectangle.rkt")

(struct region (terrain entities))

;; int int -> region
;; "Generate" a region, given its dimensions
(define (make-region width height)
  (define terrain (make-terrain width height))
  (define entities (generate-entities terrain))
  (region terrain entities))

;; void -> (listof entities)
;; Generate (well, now it's hardcoded, but...) the entities of the world
(define (generate-entities terrain)
  (define-values (x y) (random-walkable-tile-coords terrain))
  (define npc (new person% [x x] [y y] [character "&"] [color "mediumgoldenrod"]))
  (list npc))

;; terrain -> int int
;; Return a random tile which is located in a room on the given terrain
(define (random-walkable-tile-coords terrain)
  (define rooms (terrain-rooms terrain))
  (define room (random-from-list rooms))
  (values (random (rect-x room) (rect-x-bound room))
          (random (rect-y room) (rect-y-bound room))))

;; (listof val) -> val
;; Return a random value from the list
(define (random-from-list lst)
  (list-ref lst (random (length lst))))


