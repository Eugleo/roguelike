#lang racket

(require 2htdp/universe "screen.rkt" "world.rkt")

(define w 800)
(define h 608)
(define tile-size 32)

(big-bang (make-screen (new world% [width 25] [height 19]) tile-size)
  (name "The Roguelike")
  (to-draw (λ (screen) (render screen)))
  (on-key (λ (screen key) (handle-key key screen)))
  (stop-when (λ (state) #f)))

