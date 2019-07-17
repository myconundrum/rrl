#lang racket

; entity actions, typically driven by ai or user input.
(provide action-player-look 
         action-enqueue
         action-dequeue
         action-update

)

(require lens threading 
         "poslist.rkt" "pos.rkt" "field.rkt" "world.rkt" "object.rkt" "player.rkt")

;
; cast a field of view at the current player position and then explore those 
; tiles we have now seen.
; (world) -> (world)
;
(define (update-player-fov w)
  (define f (make-field (world-player-pos w) (obget (world-player w) 'light)
                        (λ (p) (not (hash-ref (obget w 'unexplored) p #f)))))
  (define o-list
    (foldl (λ (p rl) (if (field-has-pos? f p) (cons p rl) rl)) empty 
           (append (pl-all-pos (world-actors w)) (pl-all-pos (world-items w)))))
  (define nw  (lens-transform (world-player-lens w) w 
                              (λ (o) (obset o 
                                            'fov f
                                            'objects-in-fov o-list))))
  (world-explore nw (field-get-points (obget (world-player nw) 'fov))))


(define (update-player-presence w)
  (define p (world-player-lens w))
  (define of (hash-ref (p w) 'presence #f))
  (define origin (hash-ref (p w) 'pos))
  (define radius (hash-ref (p w) 'stealth))

  (lens-set p w (hash-set (p w) 'presence (if of 
                                              (field-recast of .9 origin radius)
                                              (make-field origin radius)))))


(define (action-player-look-at w apos tpos act)
  (lens-transform 
   (hash-ref-nested-lens 'actors (world-player-pos w) 'look-at-index) w 
   (λ (v) 
     (modulo (add1 v) 
             (length ((hash-ref-nested-lens 'actors apos 'objects-in-fov) w))))))

(define (action-attack w apos tpos act) w)


(define (action-player-look w) w
  (define new-fov (update-player-fov w))
  (obset (world-explore new-fov (field-get-points 
                                 (obget (world-player new-fov) 'fov)))))

(define (action-look w apos tpos act) w)





(define (action-pickup w apos tpos act)
  (define o (pl-get (world-items w) (obget act 'target-pos)))
  (if (and o (obhas? o 'flag-treasure))
      (~> 
       (lens-transform 
        (hash-ref-nested-lens 'actors apos 'gold) w
        (λ (v) (+ v ((hash-ref-nested-lens 'items tpos 'gold) w))))
       (lens-transform (hash-ref-lens 'items) _ (λ (h) (hash-remove h tpos)))) w))


(define (action-move w apos tpos act)
  (define tposc (pos-clamp tpos 
                        0 (obget w 'width) 0 (obget w 'height)))
  (define player-pos (world-player-pos w))
  (define actor (pl-get (world-actors w) apos))
 
  (cond
    ; should this be an attack instead? (to contains an actor and one of from or to
    ; is the player.)
    [(and (pl-pos? (world-actors w) tposc) 
          (or (= apos player-pos) (= tposc player-pos)))
     (action-enqueue w 'action-attack apos tposc)]

    [(and (world-valid-pos? w tposc) 
          (obhas? (pl-get (world-explored w) tposc) 'flag-passable))
     (~> w
         (lens-transform 
          world-actors-lens _ 
          (λ (h) (hash-set (hash-remove h apos) tposc (obset actor 'pos tposc))))
         (action-enqueue 'action-pickup tposc tposc)
         (obset 'player-pos (if (= apos player-pos) tposc apos)))] ;update player pos

    ; else, invalid...remove action.
    [else w]))





(define action-mapping 
  (hash 
   'action-move action-move
   'action-look-at action-player-look-at
   'action-attack action-attack
   'action-pickup action-pickup
   'action-look action-look))

(define (action-do-nothing w apos tpos act) (action-dequeue w apos))

(define (action-dequeue w a) 
  (lens-transform 
   (hash-ref-nested-lens 'actors 
                         (if (pos? a) a (obget a 'pos)) 
                         'actions) w rest))




(define (action-enqueue w act apos tpos)
  (define action (obset (ob act) 
                        'actor-pos apos
                        'target-pos tpos
                        'fn (hash-ref action-mapping act)))

  
  (lens-transform (hash-ref-nested-lens 'actors apos)  w  
                  (λ (a) 
                    (obset a 'actions 
                           (append (obget a 'actions empty) (list action))))))


(define (get-action-time w act apos)
  (+ (world-time w) (* (obget act 'speed) 
                       ((hash-ref-nested-lens 'actors apos 'speed) w))))

(define (run-one-action w a)

  (define apos (obget a 'actor-pos))
  (define tpos (obget a 'target-pos))
  (define atime (get-action-time w a apos))

  (~> ((obget a 'fn) (action-dequeue w apos) apos tpos a)
      (lens-transform (hash-ref-lens 'time) _ 
                      (λ (v) (if (< v atime) atime v)))))







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

    

