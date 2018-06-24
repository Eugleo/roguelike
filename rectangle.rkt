#lang racket

;; Basic rectangle structure used to construct rooms

(provide rect rect-x-bound rect-y-bound rect-x rect-y rect-intersects? rect-center)

(struct rect (x y w h))

;; rect -> int
;; Return the right bound of the rectangle
(define (rect-x-bound rect) (+ (rect-x rect) (rect-w rect)))

;; rect -> int
;; Return the bottom bound of the rectangle
(define (rect-y-bound rect) (+ (rect-y rect) (rect-h rect)))

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
