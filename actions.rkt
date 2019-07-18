#lang racket

; entity actions, typically driven by ai or user input.
(provide action-player-look 
         action-enqueue
         action-dequeue
         action-update
         action-cancel-modes

)

(require threading 
         "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt")

;
; cast a field of view at the current player position and then explore those 
; tiles we have now seen.
; (world) -> (world)
;
(define (update-player-fov w)
  (define f (make-field (world-player-pos w) (world-player-attribute w 'light)
                        (λ (p) (not (hash-ref (obget w 'unexplored) p #f)))))
  (define o-list
    (foldl (λ (p rl) (if (field-has-pos? f p) (cons p rl) rl)) empty 
           (append (hash-keys (world-actors w)) (hash-keys (world-items w)))))
  (define nw (world-player-transform w (λ (o) (obset o 
                                            'fov f
                                            'objects-in-fov o-list))))
  (world-explore nw (field-get-points (world-player-attribute nw 'fov))))

(define (update-player-presence w)
  (world-player-transform-attribute 
   w 'presence (λ (o v) (if v 
                            (field-recast v .9 (obget o 'pos) (obget o 'stealth))
                            (make-field (obget o 'pos) (obget o 'stealth))))))


(define (action-player-look-at w apos tpos act)
  (world-player-transform-attribute 
   w 'look-at-index 
   (λ (o v) (modulo (add1 v) (length (obget o 'objects-in-fov))))))

(define (action-cancel-modes w)
  (world-player-set-attribute w 'look-at-index -1))

(define (action-attack w apos tpos act) w)


(define (action-player-look w) w
  (define new-fov (update-player-fov w))
  (world-explore new-fov (field-get-points 
                          (world-player-attribute new-fov 'fov))))

(define (action-look w apos tpos act) w)
(define (action-sleep w apos tpos act) w)


(define (action-pickup w apos tpos act)
  (define t (world-item w tpos))
  (if (and t (obhas? t 'flag-treasure))
      (world-item-delete 
       (world-actor-transform-attribute 
        w apos 'gold (λ (o v) (+ v (obget t 'gold)))) tpos)
      w))


(define (action-move w apos tpos act)
  (define tposc (pos-clamp tpos 
                           0 (hash-ref w 'width) 0 (hash-ref w 'height)))
  (define player-pos (world-player-pos w))
 
  (cond
    ; should this be an attack instead? (to contains an actor and one of from or to
    ; is the player.)
    [(and (world-actor-at? w tposc) (or (= apos player-pos) (= tposc player-pos)))
     (action-enqueue w 'action-attack apos tposc)]
    [(and (world-valid-pos? w tposc) (obhas? (world-terrain w tposc) 'flag-passable))
     (action-enqueue (world-actor-set-attribute w apos 'pos tposc) 
                     'action-pickup tposc tposc)]
    [else w]))




(define action-mapping 
  (hash 
   'action-move action-move
   'action-look-at action-player-look-at
   'action-attack action-attack
   'action-pickup action-pickup
   'action-look action-look
   'action-sleep action-sleep))

(define (action-do-nothing w apos tpos act) (action-dequeue w apos))

(define (action-dequeue w a) 
  (world-actor-transform-attribute w a 'actions (λ (o v) (rest v))))

(define (action-enqueue w act apos tpos)
  (define action (obset (ob act) 
                        'actor-pos apos
                        'target-pos tpos
                        'fn (hash-ref action-mapping act)))

  (world-actor-transform-attribute 
   w apos 'actions (λ (o v) (append (obget o 'actions empty) (list action)))))

(define (get-action-time w act apos)
  (+ (world-time w) (* (obget act 'speed) 
                       (obget (world-actor w apos) 'speed))))

(define (run-one-action w a)

  (define apos (obget a 'actor-pos))
  (define tpos (obget a 'target-pos))
  (define atime (get-action-time w a apos))
  (define curtime (obget w 'time))

  (~> ((obget a 'fn) (action-dequeue w apos) apos tpos a)
      (obset 'time (if (< curtime atime) atime curtime))))


;
; only the player right now..
;
(define (action-update w)
  (define actions (obget (world-player w) 'actions))
  (if (or (not actions) (null? actions))
      ; After all actions have been run, update final state.
      (~> w
          (update-player-fov)
          (update-player-presence))
      (action-update (run-one-action w (first actions))))) 

    

