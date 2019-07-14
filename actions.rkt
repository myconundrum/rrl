#lang racket

; entity actions, typically driven by ai or user input.


(provide action-player-look
         action-player-move)

(require lens threading "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt")

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
           (λ (p) (not (hash-ref (ob-attribute w 'unexplored) p #f)))))])

    (world-explore new-world 
                   (field-get-points (lens-view world-player-look-lens new-world)))))


; only picks up treasure so far.
(define (action-player-pickup w)

  (define p (ob-attribute w 'player))
  (define at (ob-attribute p 'pos))
  (define o (world-object w 'items at))

  (if (and o (ob-has-flag? o 'flag-treasure))
      
      (let ([w2 (lens-set world-items-lens w 
                          (hash-remove (lens-view world-items-lens w) at))])
        (lens-set world-player-lens w2 
                  (ob-attribute-set p 'gold (+ (ob-attribute o 'gold) 
                                                (ob-attribute p 'gold))))) 
      w))

(define (action-player-attack w at) w)

; if possible, move the player.
;(world dx dy) -> world
(define (action-player-move w dx dy)

  (define p (ob-attribute w 'player))
  (define new-pos 
    (pos-clamp (pos-delta (ob-pos p) dx dy)
               0 (ob-attribute w 'width) 0 (ob-attribute w 'height)))


  (cond 
    ; if there is an actor, switch to an attack instead of a move.
    [(world-object-at-pos? w 'actors new-pos) (action-player-attack w new-pos)]
    ; if this is a valid position, move there, pick anything up, and look around.
    [(and (world-valid-pos? w new-pos) 
          (ob-has-flag? 
           (hash-ref (ob-attribute w 'explored) new-pos) 'flag-passable))

     (~> (lens-set world-player-pos-lens w new-pos) 
         (action-player-pickup)
         (action-player-look))]
    ; invalid move.
    [else w]))
