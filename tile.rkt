#lang racket

;; The basic building block of all maps

(provide tile%)

;; Will be subclassed a lot in the future
(define tile%
  (class object%
    (init-field see-through walk-through)
    (define/public (is-walk-through?) walk-through)
    
    (super-new)))
