#lang racket

;; The basic building block of all terrains

(provide tile% wooden-floor% grass% wall% door%)

(define tile%
  (class object%
    (init transparent walkable)
    
    (define is-transparent transparent)
    (define/public (transparent?) is-transparent)

    (define is-walkable walkable)
    (define/public (walkable?) is-walkable)

    (define seen #f)
    (define/public (seen?) seen)
    (define/public (set-seen! v) (set! seen v))

    (field [light 0])
    
    (super-new)))

(define wooden-floor%
  (class tile% 
    (super-new [transparent #t] [walkable #t])))

(define grass%
  (class tile% (super-new [transparent #t] [walkable #t])))

(define wall% 
  (class tile% 
    (super-new [transparent #f] [walkable #f])
    (field [orientation 'cross])))

(define door%
  (class tile% (super-new [transparent #t] [walkable #t])))
