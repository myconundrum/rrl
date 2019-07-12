#lang racket




(require lens unstable/lens racket/random 
         "object.rkt" "pos.rkt" "config.rkt" "dungeon.rkt")

(provide (struct-lenses-out world)
         (struct-out world)
         make-world
         world-explore
         world-valid-pos?)


(struct/lens world (player actors items dungeon terrain width height))

(define world-player-pos-lens
  (lens-compose object-pos-lens world-player-lens))



;(world pos) -> bool
(define (world-valid-pos? w p)
  (let ([x (pos-x p)] [y (pos-y p)])
    (and (>= x 0) (>= y 0) (< x (world-width w)) (< y (world-height w)))))

; (world list) -> world
(define (world-explore w l)
  (lens-set 
   world-terrain-lens w
   (foldl (λ (p h) 
            (hash-set h p 
                      (make-object 
                       (if (dungeon-pos-open? (world-dungeon w) p) "floor" "wall")))) 
          (lens-view world-terrain-lens w) l)))


;() -> world
(define (make-world)
  (let ([w (world (make-actor "player") 
                  empty
                  empty
                  (create-dungeon columns rows max-rooms)
                  (hash)
                  columns rows)])

    (lens-set 
     world-player-pos-lens w (dungeon-random-open-pos (world-dungeon w)))))

