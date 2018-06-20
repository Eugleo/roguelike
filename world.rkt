#lang racket

;; The world and associated functions

(require "entities.rkt" "point.rkt" "world-map.rkt")
(provide (all-defined-out))

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

