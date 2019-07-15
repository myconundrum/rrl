#lang racket

(require "pos.rkt" "object.rkt")


(provide make-pl
         ; is there already something at this position?
         pl-pos?
         ; get object at given position.
         pl-get
         ; set object at given position.
         pl-set
         ; remove object at given position
         pl-clear 
         ; return list of all set positions in this pl.
         pl-all-pos
         ; return list of all objects stored in this pl.
         pl-all-objects)

(define (make-pl) (hash))


; true if there is already a key at the given pos. 
; Note. You can query on either an object (look up pos) or a pos directly.
(define (pl-pos? pl o) (hash-has-key? pl (if (pos? o) o (obget o 'pos))))



(define (pl-get pl p) (hash-ref pl p #f))
(define (pl-set pl o) (hash-set pl (obget o 'pos) o))
(define (pl-clear pl o) (hash-remove pl (if (pos? o) o (obget o 'pos))))
(define (pl-all-pos pl) (hash-keys pl))
(define (pl-all-objects pl) (hash-values pl))
