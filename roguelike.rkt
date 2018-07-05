#lang racket/gui

;; The main game loop and the rendering code

(provide make-roguelike)

(define counter 1)

(require lux lux/chaos/gui/key "world.rkt" "entities.rkt" "region.rkt" "terrain.rkt" "tile.rkt" "fov.rkt" "image-db.rkt")

;; int int int -> roguelike
;; Construct a roguelike from a given world (canvas gets generated)
(define (make-roguelike column-no row-no tile-size)
  (define canvas (new bitmap-dc% [bitmap (make-bitmap (* column-no tile-size) (* row-no tile-size))]))
  (define world (make-world column-no row-no))

  (define player (world-player world))
  (define-values (x y) (values (send player get-x) (send player get-y)))
  (define tiles (terrain-tiles (region-terrain (world-current-region world))))
  (cast-light x y tiles)
  (roguelike world canvas tile-size))

;; The main game "loop structure"
;; Tile size is an int describing the scale between the world size and the roguelike size
(struct roguelike (world canvas tile-size)
  #:methods gen:word
  [(define (word-fps state) 0.0)

   ;; roguelike string -> string
   ;; Set the title of the created window
   (define (word-label state framerate) "Roguelike")
   
   ;; roguelike event -> roguelike
   ;; Return a new state after handling an event
   ;; event = keystroke, mouse movement, window position and/or size change
   (define (word-event state event)
     (cond
       [(key-event? event)
        (case (send event get-key-code)
          [(numpad4 #\h left) (try-move -1 0 state)]
          [(numpad6 #\l right) (try-move 1 0 state)]
          [(numpad2 #\j down) (try-move 0 1 state)]
          [(numpad8 #\k up) (try-move 0 -1 state)]
          [(numpad7 #\y #\z) (try-move -1 -1 state)]
          [(numpad9 #\u) (try-move 1 -1 state)]
          [(numpad1 #\b) (try-move -1 1 state)]
          [(numpad3 #\n) (try-move 1 1 state)]
          [(#\q) #f]
          [else state])]
       [(eq? 'close event) #f]
       [else state]))
   
   ;; roguelike -> (int int drawing-context -> void)
   ;; Given current game state, perform the necessary drawing of the game scene
   (define (word-output state)
     (define world (roguelike-world state))
     (define canvas (roguelike-canvas state))
     (define tile-size (roguelike-tile-size state))
     (define player (world-player world))
     (define region (world-current-region world))
     (define terrain (region-terrain region))
     (define tiles (terrain-tiles terrain))
    
     ;; The expected return type of word-output is a function expecting three arguments
     (lambda (width height dc)
       (send dc set-background "black")
       (send dc clear)
       (send canvas set-background "black")
       (send canvas clear)

       ;; Render walls and obstacles
       (for ([column (in-vector tiles)]
             [x (in-naturals)])
         (for ([tile (in-vector column)]
               [y (in-naturals)]) 
           (define visible (equal? 5 (get-field light tile)))
           (define seen (send tile seen?))
           (cond
             [(or visible seen)
              (send tile set-seen! #t)
              (draw-bitmap-on-tile (get-bitmap-for-tile tile visible) x y tile-size canvas)])))
       (send dc draw-bitmap (send canvas get-bitmap) 0 0)  

       ;; Render NPCs and animals
       (for ([entity (in-list (region-entities region))])
         (define x (send entity get-x))
         (define y (send entity get-y))
         (define bitmap (get-bitmap-for-entity entity))

         (cond [(equal? 5 (get-field light (get-tile x y tiles))) (draw-bitmap-on-tile bitmap x y tile-size dc)]))
       
       ;; Render the player
       (define p-bitmap (get-bitmap-for-entity player))
       (draw-bitmap-on-tile p-bitmap (send player get-x) (send player get-y) tile-size dc)))])

       


(define (draw-bitmap-on-tile bm x y tile-size canvas)
  (send canvas draw-bitmap bm (* x tile-size) (* y tile-size)))


;; int int roguelike -> roguelike
;; If the world boundaries permit, move the player as specified
(define (try-move dx dy roguelike)
  (define player (world-player (roguelike-world roguelike)))
  (define new-x (+ dx (send player get-x)))
  (define new-y (+ dy (send player get-y)))
  (define terrain (region-terrain (world-current-region (roguelike-world roguelike))))

  (define can-walk 
    (and (< -1 new-x (terrain-width terrain))
         (< -1 new-y (terrain-height terrain))
         (terrain-is-place-walk-through? new-x new-y terrain)))
  (cond [can-walk 
         (for ([column (in-vector (terrain-tiles terrain))]
               [x (in-naturals)])
           (for ([tile (in-vector column)]
                 [y (in-naturals)])            
             (set-field! light tile 0)))
         (send player move! dx dy)
         (cast-light new-x new-y (terrain-tiles terrain))])
         
  roguelike)


(define (one-from opt1 opt2)
  (if (eq? (random 2) 1) opt1 opt2))
