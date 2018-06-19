#lang racket

(require 2htdp/universe 2htdp/image)
(require "world.rkt" "point.rkt" "entities.rkt" "world-map.rkt")
(provide (all-defined-out))

(struct screen (world canvas))
(struct map-canvas (width height tile-size canvas))

; screen -> image
; used in bigbang to render the world state
(define (render screen)
  (define canvas (screen-canvas screen))
  (define tile-size (map-canvas-tile-size canvas))
  (define world (screen-world screen))
  (for/fold ([canvas (map-canvas-canvas canvas)])
            ([entity (in-list (world-entities world))])
    (define x (send entity get-x))
    (define y (send entity get-y))
    (define character (send entity get-character))
    (define color (send entity get-color))    
    (draw-on-canvas character x y color "brown" canvas tile-size)))

; key screen -> screen
; used in bigbang to handle key presses
(define (handle-key key screen)
  (case key
    [("numpad4" "h" "left") (try-move -1 0 screen)]
    [("numpad6" "l" "right") (try-move 1 0 screen)]
    [("numpad2" "j" "down") (try-move 0 1 screen)]
    [("numpad8" "k" "up") (try-move 0 -1 screen)]
    [("numpad7" "y" "z") (try-move -1 -1 screen)]
    [("numpad9" "u") (try-move 1 -1 screen)]
    [("numpad1" "b") (try-move -1 1 screen)]
    [("numpad3" "n") (try-move 1 1 screen)]
    [("q") (stop-with screen)]
    [else screen]))

; int int screen -> screen
; if the world permits, move the player as specified
(define (try-move dx dy screen)
  (define player (world-player (screen-world screen)))
  (define x (send player get-x))
  (define y (send player get-y))
  (define w-map (world-world-map (screen-world screen)))
  (define can-walk 
    (and (< -1 (+ x dx) (world-map-width w-map))
         (< -1 (+ y dy) (world-map-height w-map))
         (is-tile-walk-through? (+ x dx) (+ y dy) w-map)))
  (cond [can-walk (send player move! dx dy)])
  screen)

; character int int color color image int -> image
; draw a given character on the canvas
(define (draw-on-canvas char x y fg bg canvas tile-size)
  (define char-rect  
    (overlay 
      (text/font char 28 fg "Source Code Pro" 'script 'normal 'normal #f)
      (rectangle tile-size tile-size "solid" bg)))
  (place-image/align char-rect (* x tile-size) (* y tile-size) 'left 'top canvas))

; world int -> screen
; construct a screen from a given world (canvas gets generated)
(define (make-screen width height tile-size)
  (define world (make-world (/ width tile-size) (/ height tile-size)))
  (define canvas (make-map-canvas (world-world-map world) tile-size))
  (screen world canvas))

; world-map int -> map-canvas
; construct a canvas from a world map
(define (make-map-canvas w-map tile-size)
  (define tiled-width (world-map-width w-map))
  (define tiled-height (world-map-height w-map))
  (define starting-canvas 
    (rectangle (* tiled-width tile-size) (* tiled-height tile-size) "solid" "black"))
  (define canvas 
    (for*/fold ([canvas starting-canvas])
               ([x (in-range tiled-width)]
                [y (in-range tiled-height)])
      (define character (if (is-tile-walk-through? x y w-map) "."  "§"))
      (define color (if (is-tile-walk-through? x y w-map) "brown"  "black"))
      (draw-on-canvas character x y "white" color canvas tile-size)))
  (map-canvas tiled-width tiled-height tile-size canvas))

