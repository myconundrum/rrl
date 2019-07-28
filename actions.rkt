#lang racket

; entity actions, typically driven by ai or user input.
(provide action-player-look 
         action-enqueue
         action-dequeue
         action-update
         action-cancel-modes)

(require threading "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt" "msg.rkt")

;
; cast a field of view at the current player position and then explore those 
; tiles we have now seen.
; (world) -> (world)
;
(define (update-player-fov w)
  (define f (make-field (player-pos w) (player-attribute w 'light)
                        (λ (p) (not (hash-ref (obget w 'terrain) p #f)))))
  (define o-list
    (foldl (λ (p rl) (if (field-has-pos? f p) (cons p rl) rl)) empty 
           (append (hash-keys (actors w)) (hash-keys (items w)))))
  (define nw (player-transform w (λ (o) (obset o 
                                                     'fov f
                                                     'objects-in-fov o-list))))
  (explore nw (field-get-points (player-attribute nw 'fov))))

(define (update-player-presence w)
  (player-transform-attribute 
   w 'presence (λ (o v) (if v (field-recast v .9 (obget o 'pos) (obget o 'stealth))
                            (make-field (obget o 'pos) (obget o 'stealth))))))

(define (action-player-look-at w apos tpos act)
  (player-transform-attribute 
   w 'look-at-index 
   (λ (o v) (modulo (add1 v) (length (obget o 'objects-in-fov))))))

(define (action-cancel-modes w)
  (player-set-attribute w 'look-at-index -1))

(define (action-player-look w) w
  (define new-fov (update-player-presence (update-player-fov w)))
  (explore new-fov (field-get-points 
                          (player-attribute new-fov 'fov))))


(define msg-attack 
  "%color:actor %actor:look-desc %color:red attacks %color:target %target:look-desc %color:white for %actor:attack damage.")
(define msg-dies
  "%color:actor %actor:look-desc %color:white dies.")

(define (remove-if-dead w p)
    (if (and (< (actor-attribute w p 'hp) 1) (not (= (player-pos w) p)))
        (~>          
         (msg-queue w (msg w (actor w p) (actor w p) msg-dies))
         (actor-delete p)) w))


(define (action-attack w apos tpos act) 
  (if (actor w tpos)
      (~> (actor-transform-attribute 
           w tpos 'hp  (λ (o v) (- v (actor-attribute w apos 'attack))))
          (msg-queue (msg w (actor w apos) (actor w tpos) msg-attack))
          (remove-if-dead tpos)


) w))

(define (action-look w apos tpos act) 
  (if (= apos (player-pos w)) (action-player-look w) w))

(define (action-sleep w apos tpos act) w)

(define msg-treasure
  "%color:actor %actor:look-desc %color:white picked up %color:target %target:look-desc %color:white worth %target:gold gold.")
(define msg-item "%color:actor %actor:look-desc %color:white picked up %color:target %target:look-desc.")

(define (action-pickup w apos tpos act)
  (define t (item w tpos))
  (cond
    [(and t (obhas? t 'flag-treasure))
     (~> w 
          (actor-transform-attribute apos 'gold (λ (o v) (+ v (obget t 'gold))))
          (item-delete tpos)
          (msg-queue (msg w (actor w apos) t msg-treasure))) ]
    [t (~> w 
           (actor-set-attribute apos 'inventory (cons t (actor-attribute w apos 'inventory empty)))
           (item-delete tpos)
           (msg-queue (msg w (actor w apos) t msg-item)))]
    [else w]))

(define (action-move w apos tpos act)
  (define tposc (pos-clamp tpos 
                           0 (hash-ref w 'width) 0 (hash-ref w 'height)))
  (define ppos (player-pos w))
 
  (cond
    ; should this be an attack instead? (to contains an actor and one of from or to
    ; is the player.)
    [(and (actor-at? w tposc) (or (= apos ppos) (= tposc ppos)))
     (action-enqueue w 'action-attack apos tposc)]
    ; if this isn't attack, don't allow a move onto another actor.
    [(actor-at? w tposc) w]
    ; move if its a valid position...
    [(passable? w tposc) (action-enqueue 
                                (actor-set-attribute w apos 'pos tposc) 
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

(define (action-dequeue w a) 
  (actor-transform-attribute w a 'actions (λ (o v) (rest v))))

(define (action-enqueue w act apos tpos)
  (define action (obset (ob act) 
                        'actor-pos apos
                        'target-pos tpos
                        'timestamp (game-time w)
                        'fn (hash-ref action-mapping act)))

  (actor-transform-attribute 
   w apos 'actions (λ (o v) (append (obget o 'actions empty) (list action)))))

(define (get-action-time w act apos)
  (+ (obget act 'timestamp) (* (obget act 'speed) (obget (actor w apos) 'speed))))

(define (run-one-action w a)
  (define apos (obget a 'actor-pos))
  (define tpos (obget a 'target-pos))
  (define atime (get-action-time w a apos))
  (define curtime (obget w 'time))

  (~> ((obget a 'fn) (action-dequeue w apos) apos tpos a)
      (obset 'time (if (< curtime atime) atime curtime))))

(define (ai-best-move w apos)
  (define presence (player-attribute w 'presence))
  (define moves (list 0 0-1i 0+1i 1 -1 1+1i 1-1i -1+1i -1-1i))
  (+ apos (first (sort moves > #:key (λ (v) (if (field-has-pos? presence (+ v apos)) 
                                                (field-get-pos presence (+ v apos)) 0))
                       #:cache-keys? #t))))

(define (ai-state-searching w apos)
  (define tpos (ai-best-move w apos))
  (if (field-has-pos? (player-attribute w 'presence) tpos) 
      (action-enqueue w 'action-move apos tpos)
      (action-enqueue 
       (actor-set-attribute w apos 'state 'state-asleep)
       'action-sleep apos apos)))

(define (ai-state-asleep w apos)
  ; far too simple. If detect player, move to ai-searching. Otherwise, sleep.
  (if (field-has-pos? (player-attribute w 'presence) apos)
      (action-enqueue (actor-set-attribute w apos 'state 'state-searching) 
                      'action-move apos (ai-best-move w apos))
      (action-enqueue (actor-set-attribute w apos 'state 'state-asleep) 
                      'action-sleep apos apos)))

(define ai-states (hash 
                   'state-asleep ai-state-asleep
                   'state-searching ai-state-searching))

(define (ai-decide-action w apos)
  (if (null? (actor-attribute w apos 'actions empty)) 
      ((hash-ref ai-states 
                 (actor-attribute w apos 'state 'state-asleep)) w apos) w))

(define (get-ai-actors w)
  (filter (λ (p) (not (= (player-pos w) p))) (hash-keys (actors w))))

(define (ai-update-actions w)
  (foldl (λ (p nw) (ai-decide-action nw p)) w (get-ai-actors w)))

(define (get-player-action w)
  (define act (player-attribute w 'actions empty))
  (if (null? act) #f (first act)))

(define (sorted-ai-actors w)
  (sort (get-ai-actors w) <
        #:key (λ (p) (get-action-time w (first (actor-attribute w p 'actions)) p))
        #:cache-keys? #t))

(define (get-next-actor w)
  (define ai (first (sorted-ai-actors w)))
  (define pa (get-player-action w))
  (if (or (not pa) 
          (< (get-action-time w (first (actor-attribute w ai 'actions)) ai)
             (get-action-time w pa (player-pos w))))  ai (player-pos w)))

(define (action-update w)
  (define player-action (get-player-action w))
  (define new-world (ai-update-actions w))
  (define next-actor (get-next-actor new-world))
  (define act (first (actor-attribute new-world next-actor 'actions)))
  (define action-time (get-action-time new-world act next-actor))

  (if (or player-action (> (game-time new-world) action-time)) 
      (action-update (run-one-action new-world act))
      ; finish up player-updates after all actions.
      (update-player-presence (update-player-fov new-world))))

