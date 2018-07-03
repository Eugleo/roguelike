#lang racket

(provide cast-light)

(require "terrain.rkt")

(define vis-radius 8)

(define (cast-light x y start end start-row xx xy yx yy tiles)
  (define width (tiles-width tiles))
  (define height (tiles-height tiles))

  (define (rec-helper blocked? distance strt new-start)
    (cond
      [(and (<= distance vis-radius) (not blocked?)) 
       (helper2 #f (- distance) (- distance) new-start strt distance)]
      [else #f]))

  (define (helper2 blocked? deltaX deltaY new-start strt distance)
    (define c-x (+ x (* xx deltaX) (* xy deltaY)))
    (define c-y (+ y (* yx deltaX) (* yy deltaY )))
    (define left-slope (/ (- deltaX 0.5) (+ deltaY 0.5)))
    (define right-slope (/ (+ deltaX 0.5) (- deltaY 0.5)))
    (define c-tile (get-tile c-x c-y tiles))
    
    (cond
      [(> deltaX 0) (rec-helper blocked? (add1 distance) strt new-start)] 
      [(or (not (and (>= c-x 0) (>= c-y 0) (< c-x width) (< c-y height))) 
           (< strt right-slope))
       (helper2 blocked? (add1 deltaX) deltaY new-start strt distance)]
      [(> end left-slope) (rec-helper blocked? (add1 distance) strt new-start)]
      [else 
        
       (if (<= (get-radius deltaX deltaY) vis-radius) 
           (set-field! light c-tile 5)
           #f)
       (if blocked? 
           (if (not (send c-tile transparent?))
               (helper2 #t (add1 deltaX) deltaY right-slope strt distance)
               (helper2 #f (add1 deltaX) deltaY new-start new-start distance))
           (cond 
             [(and (< distance vis-radius) (not (send c-tile transparent?)))
              (cast-light x y strt left-slope (add1 distance) xx xy yx yy tiles)
              (helper2 #t (add1 deltaX) deltaY right-slope strt distance)]
             [else
               (helper2 #f (add1 deltaX) deltaY right-slope strt distance)]))]))

  (cond
    [(< start end) void]
    [else (rec-helper #f start-row start 0)]))

(define (get-radius x y)
  (sqrt (+ (sqr x) (sqr y))))


