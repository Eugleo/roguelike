#lang racket

(require "entities.rkt" "point.rkt" "world-map.rkt")
(provide (all-defined-out))

(struct world (world-map player entities))

(define (generate-world-map width height)
  (define w-map (make-world-map width height))
  (set-wall! '((6 3) (7 3) (8 3) (8 4) (8 5)) w-map)
  w-map)

(define (generate-entities)
  (define npc (new entity% [x 3] [y 10] [character "&"] [color "blue"]))
  (define player (new entity% [x 10] [y 7] [character "@"] [color "white"]))
  (list player npc))

(define (make-world width height)
  (define world-map (generate-world-map width height))
  (define entities (generate-entities))
  (world world-map (first entities) entities))

