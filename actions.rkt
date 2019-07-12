#lang racket

; entity actions, typically driven by ai or user input.


(provide action-player-look
         action-player-move)

(require lens "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt")

;
; cast a field of view at the current player position and then explore those 
; tiles we have now seen.
;(world) -> (world)
(define (action-player-look w)
  (let ([new-world 
         (lens-set 
          world-player-look-lens w 
          (make-field 
           (lens-view world-player-pos-lens w) 3 
           (λ (p) (not (hash-ref (ob-attribute w "unexplored") p #f)))))])

    (world-explore new-world 
                   (field-get-points (lens-view world-player-look-lens new-world)))))


; only picks up treasure so far.
(define (action-player-pick-up w)

  (define p (ob-attribute w "player"))
  (define at (ob-attribute p "pos"))
  (define o (world-object w "items" at))

  (if (and o (ob-has-flag? o "flag-treasure"))
      
      (let ([w2 (lens-set world-items-lens w 
                          (hash-remove (lens-view world-items-lens w) at))])
        (lens-set world-player-lens w2 
                  (ob-attribute-set p "gold" (+ (ob-attribute o "gold") 
                                                (ob-attribute p "gold"))))) 
      w))

; if possible, move the player.
;(world dx dy) -> world
(define (action-player-move w dx dy)
  (action-player-look   
   (let* ([p (ob-attribute w "player")]
          [new-pos (pos-clamp 
                    (pos-delta (ob-pos p) dx dy)
                    0 (ob-attribute w "width") 0 (ob-attribute w "height"))])
     (if (and (world-valid-pos? w new-pos) 
              (ob-has-flag? 
               (hash-ref (ob-attribute w "explored") new-pos) "flag-passable"))
         (action-player-pick-up (lens-set world-player-pos-lens w new-pos)) w))))
