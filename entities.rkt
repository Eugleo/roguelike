#lang racket

;; The classes for all non-terrain things in the game 

(provide person%)

;; An interface describing a movable entity, like a player or an NPC
(define movable<%>
  (interface () move!))

;; The main superclass of all entities
(define entity%
  (class* object% ()
    (init-field x y character color)

    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-character) character)
    (define/public (get-color) color)
    
    (super-new)))

;; A movable entity
(define person%
  (class* entity% (movable<%>)
    (inherit-field x y character color)
    (inherit get-x get-y get-character get-color)

    (define/public (move! dx dy)
      (set! x (+ x dx))
      (set! y (+ y dy)))
    
    (super-new)))

