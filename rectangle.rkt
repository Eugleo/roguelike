#lang racket

;; Basic rectangle structure used to construct rooms

(provide rect 
         rect-x-bound 
         rect-y-bound 
         rect-x rect-y 
         rect-intersects? 
         rect-center
         rect-w rect-h
         room-get-door-coords
         room-within-distance?)

(struct rect (x y w h) #:transparent)

;; rect -> int
;; Return the right bound of the rectangle
(define (rect-x-bound rect) (+ (rect-x rect) (rect-w rect) -1))

;; rect -> int
;; Return the bottom bound of the rectangle
(define (rect-y-bound rect) (+ (rect-y rect) (rect-h rect) -1))

;; rect rect -> bool
;; Return true if the two rectangles intersect
(define (rect-intersects? rect1 rect2)
  (and (<= (rect-x rect1) (rect-x-bound rect2))
       (>= (rect-x-bound rect1) (rect-x rect2))
       (<= (rect-y rect1) (rect-y-bound rect2))
       (>= (rect-y-bound rect1) (rect-y rect2))))

;; rect -> int int
;; Return the coords of the center of the rectangle
(define (rect-center rect)
  (values (round (/ (+ (rect-x-bound rect) (rect-x rect)) 2))
          (round (/ (+ (rect-y-bound rect) (rect-y rect)) 2))))


(define (room-get-door-coords room)
  (case (random 4)
    [(0) (values (rect-x room) (+ (rect-y room) (floor (/ (rect-h room) 2))))]
    [(1) (values (rect-x-bound room) (+ (rect-y room) (floor (/ (rect-h room) 2))))]
    [(2) (values (+ (rect-x room) (floor (/ (rect-w room) 2))) (rect-y room))]
    [(3) (values (+ (rect-x room) (floor (/ (rect-w room) 2))) (rect-y-bound room))]))

(define (room-within-distance? d room1 room2)
  (and (<= (- (rect-x room1) d) (rect-x-bound room2))
       (>= (+ (rect-x-bound room1) d) (rect-x room2))
       (<= (- (rect-y room1) d) (rect-y-bound room2))
       (>= (+ (rect-y-bound room1) d) (rect-y room2))))
