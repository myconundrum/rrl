#lang racket

; base level "game object" interface. 
; current implementation is just a hash table of text attributes.
(require json racket/hash "pos.rkt")

(provide ob 
         obget
         obset
         obclear
         obhas?
         obflip-flag)


(define (obget o a [d #f]) (hash-ref o a d))
(define (obset o . kv) (apply hash-set* o kv))
(define (obclear o a) (hash-remove o a))
(define (obhas? o a) (hash-ref o a #f))
(define (obflip-flag o f) (if (obhas? o f) (hash-remove o f) (hash-set o f #t)))


(define ob-templates (with-input-from-file "objects.db" (λ () (read-json))))

(define (ob type #:pos [at (pos 0 0)] . more-kv)
  (hash-union (apply hash (append 
                           (list 'pos at) more-kv))
              (hash-ref ob-templates type)))
