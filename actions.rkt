#lang racket


(provide action-player-look
         action-player-move
         player-look-lens
         player-pos-lens

)

(require lens "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt")

;(world) -> (world)
(define player-look-lens
  (lens-compose actor-state-fov-lens object-state-lens world-player-lens))

(define player-pos-lens
  (lens-compose object-pos-lens world-player-lens))

(define (action-player-look w)
  (let ([new-world 
         (lens-set 
          player-look-lens w 
          (make-field 
           (lens-view player-pos-lens w) 3 
           (λ (p) (not (hash-ref (world-dungeon w) p #f)))))])

    (world-explore new-world 
                   (field-get-points (lens-view player-look-lens new-world)))))




;(world dx dy) -> world
(define (action-player-move w dx dy)
  (action-player-look   
   (let* ([p (world-player w)]
          [new-pos (pos-clamp (pos-delta (object-pos p) dx dy)
                              0 (world-width w) 0 (world-height w))])
     (if (and (world-valid-pos? w new-pos) 
              (object-has-flag? (hash-ref (world-terrain w) new-pos) 'passable))
         (lens-set player-pos-lens w new-pos) w))))
