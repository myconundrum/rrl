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
  (define f (make-field (world-player-pos w) 3 
                        (λ (p) (not (hash-ref (obget w 'unexplored) p #f)))))
  (define o-list
    (foldl (λ (p rl) (if (field-has-pos? f p) (cons p rl) rl)) empty 
           (append (pl-all-pos (world-actors w)) (pl-all-pos (world-items w)))))
    
  (lens-transform (world-player-lens w) w 
                  (λ (o) (obset o 
                                'look-at-index -1
                                'fov f
                                'objects-in-fov o-list))))

(define (action-player-look-at w apos tpos act)
  (action-dequeue 
   (lens-transform 
    (hash-ref-nested-lens 'actors (world-player-pos w) 'look-at-index) w 
    (λ (v) (modulo (add1 v) 
                   (length (lens-view 
                            (hash-ref-nested-lens 
                             'actors (world-player-pos w) 'objects-in-fov) w))))) apos))

(define (action-attack w apos tpos act)
  (action-dequeue w apos))


(define (action-player-look w)
  (define new-fov (update-player-fov w))
  (obset (world-explore new-fov (field-get-points 
                                 (obget (world-player new-fov) 'fov)))))

(define (action-look w apos tpos act)
  (define new-world 
    (if (= (world-player-pos w) apos) (action-player-look w) w))
  (action-dequeue new-world apos))





(define (action-pickup w apos tpos act)
  (define o (pl-get (world-items w) (obget act 'target-pos)))
  (define nw 
    (if (and o (obhas? o 'flag-treasure))
        (~> (lens-transform 
             (hash-ref-nested-lens 'actors apos 'gold) w
             (λ (v) (+ v (lens-view (hash-ref-nested-lens 'items tpos 'gold) w ))))
            (lens-transform (hash-ref-lens 'items) _ (λ (h) (hash-remove h tpos)))) w))
  (action-dequeue nw apos))


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

     (action-enqueue (action-dequeue w apos) 'action-attack apos tposc)]

    [(and (world-valid-pos? w tposc) 
          (obhas? (pl-get (world-explored w) tposc) 'flag-passable))
     (~> w
         (lens-transform 
          world-actors-lens _ 
          (λ (h) (hash-set (hash-remove h apos) tposc (obset actor 'pos tposc))))
         (action-dequeue tposc)
         (action-enqueue 'action-pickup tposc tposc)
         (action-enqueue 'action-look tposc tposc)
         (obset 'player-pos (if (= apos player-pos) tposc apos)))] ;update player pos

    ; else, invalid...remove action.
    [else (action-dequeue w apos)]))





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


;
; only the player right now..
;
(define (action-update w)
  (define actions (obget (world-player w) 'actions))
  (if (or (not actions) (null? actions)) w
      (let ([a (first actions)])
        (action-update 
         ((obget a 'fn) w (obget a 'actor-pos) (obget a 'target-pos) a))))) 

    

