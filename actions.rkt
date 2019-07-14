#lang racket

; entity actions, typically driven by ai or user input.


(provide action-player-look
         action-player-look-at
         action-player-move)

(require lens threading "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt")

;
; cast a field of view at the current player position and then explore those 
; tiles we have now seen.
;(world) -> (world)



(define (update-player-fov w)
  (define f 
    (make-field (world-player-pos w) 3 
                (λ (p) (not (hash-ref (ob-attribute w 'unexplored) p #f)))))
  (define o-list
    (foldl (λ (p rl) (if (field-has-pos? f p) (cons p rl) rl)) empty 
           (append  (hash-keys (world-actors w)) (hash-keys (world-items w)))))

  (world-update-player w (ob-attribute-set (world-player w)
                                           'look-at-index -1
                                           'fov f
                                           'objects-in-fov o-list)))

(define (action-player-look w)
  (define new-world (update-player-fov w))
  (world-explore new-world
                 (field-get-points (ob-attribute (world-player new-world) 'fov))))


; only picks up treasure so far.
(define (action-player-pickup w)

  (define p (world-player w))
  (define at (world-player-pos w))
  (define o (world-object w 'items at))

  (if (and o (ob-has-flag? o 'flag-treasure))
      
      (let ([w2 (lens-set world-items-lens w 
                          (hash-remove (lens-view world-items-lens w) at))])
        (world-update-player w2 
                  (ob-attribute-set p 'gold (+ (ob-attribute o 'gold) 
                                                (ob-attribute p 'gold))))) 
      w))

(define (action-player-look-at w)
  (define p (world-player w))
  (world-update-player 
   w (ob-attribute-set p 'look-at-index
                       (modulo (add1 (ob-attribute p 'look-at-index)) 
                               (length (ob-attribute p 'objects-in-fov))))))

(define (action-player-attack w at) w)

; if possible, move the player.
;(world dx dy) -> world
(define (action-player-move w dx dy)

  (define p (world-player w))
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

     (~> (world-update-player w (ob-attribute-set p 'pos new-pos)) 
         (action-player-pickup)
         (action-player-look))]
    ; invalid move.
    [else w]))
