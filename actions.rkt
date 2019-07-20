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
  (define new-fov (update-player-presence (update-player-fov w)))
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
                        'timestamp (world-time w)
                        'fn (hash-ref action-mapping act)))

  (world-actor-transform-attribute 
   w apos 'actions (λ (o v) (append (obget o 'actions empty) (list action)))))

(define (get-action-time w act apos)
  (+ (obget act 'timestamp) (* (obget act 'speed) 
                               (obget (world-actor w apos) 'speed))))

(define (run-one-action w a)

  (define apos (obget a 'actor-pos))
  (define tpos (obget a 'target-pos))
  (define atime (get-action-time w a apos))
  (define curtime (obget w 'time))

  (println (format "~a at ~a is running ~a" 
                   (world-actor-attribute w apos 'rep)
                   apos
                   (obget a 'fn)))

  (~> ((obget a 'fn) (action-dequeue w apos) apos tpos a)
      (obset 'time (if (< curtime atime) atime curtime))))







;
; only the player right now..
;
(define (action-update-old w)
  (define player-actions (obget (world-player w) 'actions))
 
  (if (or (not player-actions) (null? player-actions))
      ; After all actions have been run, update final state.
      (~> w
          (update-player-fov)
          (update-player-presence))
      (action-update (run-one-action w (first player-actions))))) 

(define ai-candidate-moves (list 0-1i 0+1i 1 -1 1+1i 1-1i -1+1i -1-1i))

(define (ai-best-move w apos)
  (define presence (world-player-attribute w 'presence))
  (foldl (λ (p m) (if (and (field-has-pos? presence (+ p apos))
                           (> (field-get-pos presence (+ p apos)) 
                              (field-get-pos presence m)))
                      (+ p apos) m))
         apos ai-candidate-moves))



(define (ai-state-searching w apos)
  (define tpos (ai-best-move w apos))
  (if (field-has-pos? (world-player-attribute w 'presence) tpos) 
      (action-enqueue w 'action-move apos tpos)
      (action-enqueue 
       (world-actor-set-attribute w apos 'state 'state-asleep)
       'action-sleep apos apos)))

(define (ai-state-asleep w apos)
  ; far too simple. If detect player, move to ai-searching. Otherwise, sleep.
  (if (field-has-pos? (world-player-attribute w 'presence) apos)
      (let ([nw 
             (action-enqueue 
              (world-actor-set-attribute w apos 'state 'state-searching) 
              'action-move apos (ai-best-move w apos))])
        nw)
      (action-enqueue
       (world-actor-set-attribute w apos 'state 'state-asleep) 'action-sleep apos apos)))

(define ai-states
  (hash 
   'state-asleep ai-state-asleep
   'state-searching ai-state-searching))

(define (ai-decide-action w apos)
  

  (define cur-state (world-actor-attribute w apos 'state 'state-asleep))
  ((hash-ref ai-states cur-state) w apos))




(define (ai-update-actions w)
  (define ppos (world-player-pos w))
  (foldl (λ (p nw)
           (if (= ppos p) nw (ai-decide-action nw p))) w (hash-keys (world-actors w))))

;
; ba is best-action (shortest amount of time before completion)
; while (time ba)  < cur-time or player has actions
;  run best-action
;  update ai
;  loop

(define (get-player-action w)
  (define act (world-player-attribute w 'actions empty))
  (if (null? act) #f (first act)))


(define (get-ai-actors w)
  (filter (λ (p) (not (= (world-player-pos w) p))) (hash-keys (world-actors w))))


(define (sorted-ai-actors w)

  (sort (get-ai-actors w) <
        #:key (λ (p) (get-action-time w (first (world-actor-attribute w p 'actions)) p))
        #:cache-keys? #t))



(define (get-next-actor w)
  (define best-ai (first (sorted-ai-actors w)))
  (define pa (get-player-action w))

  (cond
    [(not pa)  best-ai]
    [(< (get-action-time w (first (world-actor-attribute w best-ai 'actions)) best-ai)
        (get-action-time w pa (world-player-pos w))) best-ai]
    [else (world-player-pos w)]))




(define (action-update w)
  (define player-action (get-player-action w))
  (define new-world (ai-update-actions w))
  (define next-actor (get-next-actor new-world))
  (define act (first (world-actor-attribute new-world next-actor 'actions)))
  (define action-time (get-action-time new-world act next-actor))

  (if (or player-action (> (world-time new-world) action-time)) 
      (action-update (run-one-action new-world act))
      ; finish up player-updates after all actions.
      (update-player-presence (update-player-fov new-world))))

(define (action-tester)

  (ai-update-actions (action-player-look (make-world)))

)
