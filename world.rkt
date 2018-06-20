#lang racket

;; The world and associated functions

(provide world-player world-map world-world-map world-entities make-world)

(require "entities.rkt" "world-map.rkt")

;; The world is the datastructure holding the entities of the game, as well as the whole environment
(struct world (world-map player entities))

;; Generate (well, now it's hardcoded, but...) the entities of the world
(define (generate-entities)
  (define npc (new entity% [x 10] [y 6] [character "&"] [color "mediumgoldenrod"]))
  (list npc))

(define (make-world width height)
  (define world-map (generate-world-map width height)) ; Getting ready for generated content
  (define entities (generate-entities)) ; Getting ready for generated content
  (world world-map (new entity% [x 0] [y 0] [character "@"] [color "white"]) entities))

