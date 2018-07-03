#lang racket

;; The main file of my roguelike

(require lux lux/chaos/gui "roguelike.rkt")

;; The GUI dimensions
(define column-no 40)
(define row-no 30)
(define tile-size 16)

;; The drawing canvas is high only (- H toolbar-height), but we want it H high 
;; In other words, this is a hackish workaround
(define toolbar-height 22) 

;; The game loop
(call-with-chaos
 (make-gui #:width (* column-no tile-size) #:height (+ toolbar-height (* row-no tile-size))) 
 (lambda () (fiat-lux (make-roguelike column-no row-no tile-size)))) 

