#lang racket


(provide (struct-out object)
         (struct-lenses-out object)
         (struct-out item-state)
         (struct-lenses-out item-state)
         (struct-out actor-state)
         (struct-lenses-out actor-state)
         make-object
         make-actor
         object-at-pos?
         object-has-flag?
         object-rep
         object-color

)

(require lens unstable/lens "pos.rkt")


(struct/lens object (base pos state) #:transparent)

(struct/lens item-state (gold) #:transparent)
(struct/lens actor-state (fov gold) #:transparent)

(define (make-actor type [at (pos 0 0)])
  (make-object type at (actor-state empty 0)))



; (string pos state) -> object
(define (make-object base [at (pos 0 0)] [state #f])
  (object base at state))

; (object pos) -> bool
(define (object-at-pos? o p) 
  (= (object-pos o) p))

; (object flag-id) -> bool
(define (object-has-flag? o f) 
  (ormap (λ (flag) (eq? f flag)) (base-flags (hash-ref object-templates (object-base o)))))

; (object) -> string
(define (object-rep o) 
  (base-rep (hash-ref object-templates (object-base o))))

; (object) -> string
(define (object-color o) 
  (base-color (hash-ref object-templates (object-base o))))


(struct base (rep color flags) #:transparent)
(define object-templates 
  (hash
   "player" (base "@" "green" empty)
   "floor" (base "." "white" (list 'passable 'transparent))
   "wall" (base "#" "white" empty)))
