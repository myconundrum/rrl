#lang racket


(provide (struct-out object)
         make-object
         object-at-pos?
         object-has-flag?
         object-rep
         object-color
         object-setpos
         object-setbase
         object-setstate

)

(require "pos.rkt")


(struct object (base pos state) #:transparent)


;(object pos) -> object
(define (object-setpos o p)
  (object (object-base o) p (object-state o)))

;(object string) -> object
(define (object-setbase o b)
  (object b (object-pos o) (object-state o)))

;(object state) -> object
(define (object-setstate o s)
  (object (object-base o) (object-pos o) s))

; (string pos state) -> object
(define (make-object base [at (pos 0 0)] [state #f])
  (object base at state))

; (object pos) -> bool
(define (object-at-pos? o p) 
  (= (object-pos o) p))

; (object flag-id) -> bool
(define (object-has-flag? o f) 
  (ormap (λ (flag) (eq? f flag)) (base-flags (object-base o))))

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
