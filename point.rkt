#lang racket

(provide (all-defined-out))

(struct point (x y) #:transparent)

(define (distance pointA pointB)
  (define (square a) (* a a))
  (sqrt (+ (square (- (point-x pointA) (point-x pointB)))
           (square (- (point-y pointA) (point-y pointB))))))

(define (move-point pt x y)
  (point (+ (point-x pt) x) (+ (point-y pt) y)))

