#lang racket

;; The main file of my roguelike

(require lux lux/chaos/gui "screen.rkt")

;; The GUI dimensions
(define w 800)
(define h 608)

;; The size of one tile - i.e. the map will be (width / tile-size) tiles long
(define tile-size 32)

;; The drawing canvas is high only (H - toolbar-height), but we want it H high (hackish workaround)
(define toolbar-height 22) 

;; The game loop
(call-with-chaos
 (make-gui #:width w #:height (+ toolbar-height h)) ; Construct a GUI window
 (lambda () (fiat-lux (make-roguelike w h tile-size)))) ; Generate the inital state and run the loop

