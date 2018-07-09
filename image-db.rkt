#lang racket/gui

(provide get-bitmap-for-tile get-bitmap-for-entity)

(require "tile.rkt" "entities.rkt")

(define tiles-bitmap (read-bitmap  "tiles.png"))

(define (col-eq? col1 col2)
  (and
    (equal? (send col1 red) (send col2 red))
    (equal? (send col1 green) (send col2 green))
    (equal? (send col1 blue) (send col2 blue))
    (equal? (send col1 alpha) (send col2 alpha))))

(define (get-bitmap-for-tile tile visible)
      (define bitmap-hash (if visible tile-bitmaps-hash shadow-tile-bitmaps-hash))
      (cond
       [(is-a? tile wall%)
        (case (get-field orientation tile)
          [(horizontal) (hash-ref bitmap-hash 'wall-horizontal)]
          [(vertical) (hash-ref bitmap-hash 'wall-vertical)]
          [(up-right) (hash-ref bitmap-hash 'wall-top-right)]
          [(up-left) (hash-ref bitmap-hash 'wall-top-left)]
          [(down-right) (hash-ref bitmap-hash 'wall-bottom-right)]
          [(down-left) (hash-ref bitmap-hash 'wall-bottom-left)]
          [(cross) (hash-ref bitmap-hash 'wall-cross)]
          [(t-up) (hash-ref bitmap-hash 'wall-t-top)]
          [(t-right) (hash-ref bitmap-hash 'wall-t-right)]
          [(t-down) (hash-ref bitmap-hash 'wall-t-bottom)]
          [(t-left) (hash-ref bitmap-hash 'wall-t-left)])]
       [(is-a? tile wooden-floor%) (hash-ref bitmap-hash 'wooden-floor)]
       [(is-a? tile grass%) (hash-ref bitmap-hash 'grass)]
       [(is-a? tile door%) (hash-ref bitmap-hash 'door-open)]))


(define (get-shadow-bitmap-for-tile tile)
      (cond
       [(is-a? tile wall%)
        (case (get-field orientation tile)
          [(horizontal) (hash-ref tile-bitmaps-hash 'wall-horizontal)]
          [(vertical) (hash-ref tile-bitmaps-hash 'wall-vertical)]
          [(up-right) (hash-ref tile-bitmaps-hash 'wall-top-right)]
          [(up-left) (hash-ref tile-bitmaps-hash 'wall-top-left)]
          [(down-right) (hash-ref tile-bitmaps-hash 'wall-bottom-right)]
          [(down-left) (hash-ref tile-bitmaps-hash 'wall-bottom-left)]
          [(cross) (hash-ref tile-bitmaps-hash 'wall-cross)]
          [(t-up) (hash-ref tile-bitmaps-hash 'wall-t-top)]
          [(t-right) (hash-ref tile-bitmaps-hash 'wall-t-right)]
          [(t-down) (hash-ref tile-bitmaps-hash 'wall-t-bottom)]
          [(t-left) (hash-ref tile-bitmaps-hash 'wall-t-left)])]
       [(is-a? tile wooden-floor%) (hash-ref tile-bitmaps-hash 'wooden-floor)]
       [(is-a? tile grass%) (hash-ref tile-bitmaps-hash 'grass)]
       [(is-a? tile door%) (hash-ref tile-bitmaps-hash 'door-open)]))


(define tile-bm-coords-hash
  (make-immutable-hash
   '([wall-top-right . (5 0)]
     [wall-bottom-right . (0 0)]
     [wall-top-left . (4 0)]
     [wall-bottom-left . (1 0)]
     [wall-t-top . (1 1)]
     [wall-t-bottom . (0 1)]
     [wall-t-left . (2 1)]
     [wall-t-right . (3 1)]
     [wall-cross . (4 1)]
     [wall-horizontal . (2 0)]
     [wall-vertical . (3 0)]
     [grass . (7 0)]
     [door-open . (6 0)]
     [wooden-floor . (9 0)])))

(define tile-colors-hash
  (make-immutable-hash
    '([wooden-floor . "peru"]
      [grass . "limegreen"]
      [door-open . "LightSlateGray"]
      [wall-top-right . "LightSlateGray"]
      [wall-bottom-right . "LightSlateGray"]
      [wall-top-left . "LightSlateGray"]
      [wall-bottom-left . "LightSlateGray"]
      [wall-t-top . "LightSlateGray"]
      [wall-t-bottom . "LightSlateGray"]
      [wall-t-left . "LightSlateGray"]
      [wall-t-right . "LightSlateGray"]
      [wall-cross . "LightSlateGray"]
      [wall-horizontal . "LightSlateGray"]
      [wall-vertical . "LightSlateGray"])))

(define (get-bitmap-for-entity entity)
    (cond
      [(is-a? entity player%) (hash-ref entity-bitmaps-hash 'player)]
      [(is-a? entity person%) (hash-ref entity-bitmaps-hash 'npc)]))


(define entity-bm-coords-hash
  (make-immutable-hash
   '([player . (0 4)]
     [npc . (0 3)]
     [monster  . (13 4)])))

(define (make-colored-tile-bm x y color)
  (define dc (new bitmap-dc% [bitmap (make-bitmap 16 16)]))
  (send dc draw-bitmap-section tiles-bitmap 0 0 (* 16 x) (* 16 y) 16 16)
  (for* ([x (in-range 16)]
         [y (in-range 16)])
    (define pix-color (make-object color% 0 0 0))
    (send dc get-pixel x y pix-color)
    (cond [(col-eq? pix-color (send the-color-database find-color "white"))
           (send dc set-pixel x y (send the-color-database find-color color))]))
  (send dc get-bitmap))


(define tile-bitmaps-hash
    (for/hash ([key (in-list (hash-keys tile-bm-coords-hash))])
      (define coords (hash-ref tile-bm-coords-hash key))
      (define color (hash-ref tile-colors-hash key))
      (values key (make-colored-tile-bm (first coords) (second coords) color))))

(define shadow-tile-bitmaps-hash
    (for/hash ([key (in-list (hash-keys tile-bm-coords-hash))])
      (define coords (hash-ref tile-bm-coords-hash key))
      (values key (make-colored-tile-bm (first coords) (second coords) (if (equal? key 'grass) "DarkGreen" "Dim Gray")))))

(define entity-bitmaps-hash
    (for/hash ([key (in-list (hash-keys entity-bm-coords-hash))])
      (define coords (hash-ref entity-bm-coords-hash key))
      (define color "white")
      (values key (make-colored-tile-bm (first coords) (second coords) color))))
