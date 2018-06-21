#lang racket

;; A piece of world, should fit one screen

(provide make-region region-entities region-terrain)

(require "tile.rkt" "terrain.rkt" "entities.rkt")

(struct region (terrain entities))

;; int int -> region
;; "Generate" a region, given its dimensions
(define (make-region width height)
  (define terrain (make-terrain width height))
  (define entities (generate-entities))
  (region terrain entities))

;; void -> (listof entities)
;; Generate (well, now it's hardcoded, but...) the entities of the world
(define (generate-entities)
  (define npc (new person% [x 10] [y 6] [character "&"] [color "mediumgoldenrod"]))
  (list npc))



