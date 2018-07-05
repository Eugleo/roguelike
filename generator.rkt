#lang racket

(provide village-generator%)

(require "rectangle.rkt" "tile.rkt")

(define generator
  (interface () generate-tiles))

(define village-generator%
  (class* object% (generator)
    (super-new)
    
    (init-field width height)

    (define/public (generate-tiles width height)

      (define rooms 
        (generate-rooms 3 #:size-bounds (list 7 12) #:position-bounds (list width height)))
      (tiles-add-doors (tiles-add-rooms (make-empty-tiles width height) rooms) rooms)
      (for/fold ([tiles (make-empty-tiles width height)])
                ([x (in-list (list 7 8 9 7 8 9 7 8 9 12 15   8 9))]
                 [y (in-list (list 7 7 7 8 8 8 9 9 9 9  9   10 10))])
        (tiles-add-wall tiles x y)))


    (define (tiles-add-wall tiles x y)
      (tiles-set x y (new wall%) tiles))

    (define (tiles-add-doors tiles rooms)
      (for/fold ([acc tiles])
                ([room (in-list rooms)])
        (define-values (x y) (room-get-door-coords room))
        (tiles-set x y (new door%) acc)))
    
    (define (tiles-add-rooms tiles rooms)
      (for/fold ([acc tiles] #:result (set-walls-orientation acc))
                ([room rooms])
        (tiles-add-room acc room)))

    (define (tiles-add-room tiles room)
      (define tiles/walls
        (for*/fold ([acc tiles])
                   ([x (in-range (rect-x room) (add1 (rect-x-bound room)))]
                    [y (in-range (rect-y room) (add1 (rect-y-bound room)))])
          (tiles-set x y (new wall%) acc)))
      (for*/fold ([acc tiles/walls])
                 ([x (in-range (add1 (rect-x room)) (rect-x-bound room))]
                  [y (in-range (add1 (rect-y room)) (rect-y-bound room))])
        (tiles-set x y (new wooden-floor%) acc)))
    
    (define (make-empty-tiles width height)
      (for/vector #:length width ([xi (in-range width)])
        (for/vector #:length height ([yi (in-range height)])
          (new grass%))))

    ;; int (int, int) (int, int) -> (list rect)
    ;; Generate a given number of rooms of random size and position
    (define (generate-rooms number #:size-bounds size-bounds #:position-bounds pos-bounds)
      (define min-size (first size-bounds))
      (define max-size (second size-bounds))
      (define max-x (first pos-bounds))
      (define max-y (second pos-bounds))
      (for/fold ([rooms '()])
                ([i (in-naturals)] #:break (= (length rooms) number))
        (define width (random min-size (add1 max-size)))
        (define height (random min-size (add1 max-size)))
        (define x (random 1 (- max-x width)))
        (define y (random 1 (- max-y height)))
        (define room (rect x y width height))
        (cond
          [(all ((curry (compose not room-within-distance?)) 4 room) rooms) (cons room rooms)]
          [else rooms])))

    (define (set-walls-orientation tiles)
      (for* ([x (in-range (tiles-width tiles))]
             [y (in-range (tiles-height tiles))]
             #:when (is-a? (get-tile x y tiles) wall%))
        (define top (get-tile x (sub1 y) tiles))
        (define right (get-tile (add1 x) y tiles))
        (define bottom (get-tile x (add1 y) tiles))
        (define left (get-tile (sub1 x) y tiles))
        (define center (get-tile x y tiles))
        (cond
          [(and (is-a? top wall%) (is-a? bottom wall%) (is-a? left wall%) (is-a? right wall%)) 
           (set-field! orientation center 'cross)]
          [(and (is-a? top wall%) (is-a? bottom wall%) (is-a? left wall%)) 
           (set-field! orientation center 't-left)]
          [(and (is-a? top wall%) (is-a? bottom wall%) (is-a? right wall%)) 
           (set-field! orientation center 't-right)]
          [(and (is-a? top wall%) (is-a? left wall%) (is-a? right wall%)) 
           (set-field! orientation center 't-up)]
          [(and (is-a? left wall%) (is-a? bottom wall%) (is-a? right wall%)) 
           (set-field! orientation center 't-down)]
          [(and (is-a? top wall%) (is-a? bottom wall%)) (set-field! orientation center 'vertical)]
          [(and (is-a? left wall%) (is-a? right wall%)) (set-field! orientation center 'horizontal)]
          [(and (is-a? left wall%) (is-a? top wall%)) (set-field! orientation center 'up-left)]
          [(and (is-a? right wall%) (is-a? top wall%)) (set-field! orientation center 'up-right)]
          [(and (is-a? right wall%) (is-a? bottom wall%)) (set-field! orientation center 'down-right)]
          [(and (is-a? left wall%) (is-a? bottom wall%)) (set-field! orientation center 'down-left)]
          [(or (is-a? top wall%) (is-a? bottom wall%)) (set-field! orientation center 'vertical)]
          [(or (is-a? left wall%) (is-a? right wall%)) (set-field! orientation center 'horizontal)]))
      tiles)

    ;; int int terrain -> tile
    ;; Return the tile on the given coords
    (define (get-tile x y tiles)
      (if (and (< -1 x (vector-length tiles)) 
               (< -1 y (vector-length (vector-ref tiles 0))))
          (vector-ref (vector-ref tiles x) y)
          #f))

    (define (tiles-set x y tile tiles)
      (define tiles-copy (vector-copy tiles))
      (vector-set! (vector-ref tiles-copy x) y tile)
      tiles-copy)

    ;; (matrix tile) -> int
    ;;; Return the width of the map
    (define (tiles-width tiles)
      (vector-length tiles))

    ;; (matrix tile) -> int
    ;;; Return the height of the map
    (define (tiles-height tiles)
      (if (> (tiles-width tiles) 0) (vector-length (vector-ref tiles 0)) 0))

    (define (all predicate lst)
      (for/and ([item (in-list lst)]) (predicate item)))

    (define (mean n m)
      (/ (+ n m) 2))))
