#lang racket

(require 2htdp/universe "screen.rkt")

(define w 800)
(define h 608)
(define tile-size 32)

(big-bang (make-screen w h tile-size)
  (name "The Roguelike")
  (to-draw (λ (screen) (render screen)))
  (on-key (λ (screen key) (handle-key key screen)))
  (stop-when (λ (state) #f)))

