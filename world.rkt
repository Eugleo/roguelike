#lang racket

(require "entities.rkt" "point.rkt" "world-map.rkt")
(provide (all-defined-out))

(define world%
  (class object% 
    (init-field width height)
    (define/public (get-width) width)
    (define/public (get-height) height)

    (define player 
      (new entity% 
           [x (round (/ width 2))] 
           [y (round (/ height 2))] 
           [character "@"] 
           [color "white"]))
    (define/public (get-player) player)

    (define world-map 
      (let ([temp (new world-map% [width width] [height height])])
        (send temp set-wall! '((6 3) (7 3) (8 3) (8 4) (8 5)))
        temp))
    (define/public (get-world-map) world-map)

    (define npc (new entity% [x 3] [y 10] [character "&"] [color "blue"]))
    (define entities (list npc player))
    (define/public (get-entities) entities)

    (super-new)))
