#lang racket

;; The main superclass for all non-terrain things in the game 

(provide entity%)

(define movable<%>
  (interface () move!))

(define entity%
  (class* object% (movable<%>) 
    (init-field x y character color)

    (define/public (get-x) x)
    (define/public (get-y) y)
    (define/public (get-character) character)
    (define/public (get-color) color)

    (define/public (move! dx dy)
      (set! x (+ x dx))
      (set! y (+ y dy)))
    
    (super-new)))

