#lang racket

; base level "game object" interface. 
; current implementation is just a hash table of text attributes.
(require json lens racket/hash "pos.rkt")

(provide ob
         ob-attribute-clear 
         ob-attribute-set
         ob-flag-clear
         ob-flag-set
         ob-attribute
         ob-has-flag?
         ob-pos 
         ob-rep 
         ob-color 
         ob-make-lens
         ob-pos-lens 
         ob-rep-lens 
         ob-color-lens 
         ob-fov-lens)

(define (ob-attribute-clear o a) (hash-remove o a))
(define (ob-attribute-set o a v) (hash-set o a v))
(define (ob-flag-clear o f) (hash-remove o f))
(define (ob-flag-set o f) (hash-set o f #t))
(define (ob-attribute o a) (hash-ref o a #f))
(define (ob-has-flag? o f) (hash-has-key? o f))
(define (ob-pos o) (hash-ref o 'pos))
(define (ob-rep o) (hash-ref o 'rep))
(define (ob-color o) (hash-ref o 'color))

(define (ob-make-lens k) (hash-ref-lens k))
(define ob-pos-lens (hash-ref-lens 'pos))
(define ob-rep-lens (hash-ref-lens 'rep))
(define ob-color-lens (hash-ref-lens 'color))
(define ob-fov-lens (hash-ref-lens 'fov))


(define ob-templates
  (with-input-from-file "objects.db" (λ () (read-json))))

(define (ob type #:pos [at (pos 0 0)] . more-kv)
  (hash-union (apply hash (append (list 'pos at) more-kv))
              (hash-ref ob-templates type)))
