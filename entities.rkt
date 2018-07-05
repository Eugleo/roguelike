#lang racket

;; The classes for all non-terrain things in the game

(provide person% player%)

;; An interface describing a movable entity, like a player or an NPC
(define movable<%>
  (interface () move!))

;; The main superclass of all entities
(define entity%
  (class object%
    (init-field x y)

    (define/public (get-x) x)
    (define/public (get-y) y)

   (super-new)))

;; A movable entity
(define person%
  (class* entity% (movable<%>)
    (inherit-field x y)

    (define/public (move! dx dy)
      (set! x (+ x dx))
      (set! y (+ y dy)))

    (super-new)))

;; A movable entity
(define player%
  (class* entity% (movable<%>)
    (inherit-field x y)

    (define/public (move! dx dy)
      (set! x (+ x dx))
      (set! y (+ y dy)))

    (super-new)))
