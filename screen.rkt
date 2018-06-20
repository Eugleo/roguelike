#lang racket

;; The main game loop and the rendering code

(provide make-screen)

(require lux lux/chaos/gui/key racket/draw)
(require "world.rkt" "point.rkt" "entities.rkt" "world-map.rkt")

;; world int -> screen
;; construct a screen from a given world (canvas gets generated)
(define (make-screen width height tile-size)
  (define world (make-world (/ width tile-size) (/ height tile-size)))
  (screen world tile-size))

;; The main game "loop structure"
;; Tile size is an int describing the scale between the world size and the screen size
(struct screen (world tile-size)
  #:methods gen:word
  [(define (word-fps word) 0.0)

   (define (word-label word framerate) "Roguelike")
   
   ;; screen event -> screen
   ;; Return a new state after handling an event
   ;; event = keystroke, mouse movement, window position and/or size change
   (define (word-event word event)
     (cond
       [(key-event? event)
        (case (send event get-key-code)
          [(numpad4 #\h left) (try-move -1 0 word)]
          [(numpad6 #\l right) (try-move 1 0 word)]
          [(numpad2 #\j down) (try-move 0 1 word)]
          [(numpad8 #\k up) (try-move 0 -1 word)]
          [(numpad7 #\y #\z) (try-move -1 -1 word)]
          [(numpad9 #\u) (try-move 1 -1 word)]
          [(numpad1 #\b) (try-move -1 1 word)]
          [(numpad3 #\n) (try-move 1 1 word)]
          [(#\q) #f]
          [else word])]
       [(eq? 'close event) #f]
       [else word]))
   
   ;; screen -> (int int drawing-context -> void)
   ;; Given current game state, perform the necessary drawing of the game scene
   (define (word-output screen)
     (define font (make-font #:size 22 #:face "Press Start 2P" #:family 'default #:weight 'bold))
     (define world (screen-world screen))
     (define tile-size (screen-tile-size screen))
     (define player (world-player (screen-world screen)))
    
     ;; The expected return type of word-output is a function with three arguments
     (lambda (width height dc)
       (send dc set-background "black")
       (send dc clear)
       (send dc set-font font)

       ;; Render walls and obstacles
       (for* ([x (in-range (/ width tile-size))]
              [y (in-range (/ height tile-size))])
         (cond [(not (is-tile-walk-through? x y (world-world-map world)))
                (draw-centered-text dc "#" x y tile-size)]))

       ;; Render NPCs and animals
       (for ([entity (in-list (world-entities world))])
         (define char (send entity get-character))
         (define color (send entity get-color))
         (define x (send entity get-x))
         (define y (send entity get-y))
         (draw-centered-text dc char x y tile-size #:color color))
       
       ;; Render the player
       (draw-centered-text dc "@" (send player get-x) (send player get-y) tile-size)))])

;; Center text in its given position 
(define (draw-centered-text dc text x y tile-size #:color [color "white"])
  (send dc set-text-foreground color)
  (define-values (w h d a) (send dc get-text-extent text))
  (send dc draw-text text (+ (/ (- tile-size h) 2) (* tile-size x))
                          (+ (/ (- tile-size h d) 2) (* tile-size y))))

;; int int screen -> screen
;; If the world boundaries permit, move the player as specified
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



