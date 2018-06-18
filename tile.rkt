#lang racket

(provide (all-defined-out))

(define tile%
  (class object%
    (init-field see-through walk-through)
    (define/public (is-walk-through?) walk-through)
    
    (super-new)))
