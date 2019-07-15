#lang racket

; entity actions, typically driven by ai or user input.


(provide action-player-look
         action-player-look-at
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
  (define f 
    (make-field (world-player-pos w) 3 
                (λ (p) (not (hash-ref (obget w 'unexplored) p #f)))))
  (define o-list
    (foldl (λ (p rl) (if (field-has-pos? f p) (cons p rl) rl)) empty 
           (append (pl-all-pos (world-actors w)) (pl-all-pos (world-items w)))))

  (world-update-player w (obset (world-player w)
                                           'look-at-index -1
                                           'fov f
                                           'objects-in-fov o-list)))





(define (action-player-look-at w)
  (define p (world-player w))
  (world-update-player 
   w (obset p 'look-at-index
                       (modulo (add1 (obget p 'look-at-index)) 
                               (length (obget p 'objects-in-fov))))))






(define (action-attack w act)
  (define actor (pl-get (world-actors w) (obget act 'actor-pos))) 
  (obset w 'actors (pl-set (world-actors w) (action-dequeue actor))))


(define (action-player-look w)
  (define new-fov (update-player-fov w))
  (obset (world-explore new-fov (field-get-points 
                                 (obget (world-player new-fov) 'fov)))))
 

(define (action-look w act)
  
  (define new-world 
    (if (= (world-player-pos w) (obget act 'actor-pos)) 
        (action-player-look w)
        w))
  (define actor (pl-get (world-actors new-world) (obget act 'actor-pos)))


  (obset new-world 'actors (pl-set (world-actors new-world) (action-dequeue actor))))


(define (action-pickup w act)

  (define actor (action-dequeue (pl-get (world-actors w) (obget act 'actor-pos)))) 
  (define o (pl-get (world-items w) (obget act 'target-pos)))

  (if (and o (obhas? o 'flag-treasure))
      (obset w
             'items (pl-clear (world-items w) o)
             'actors (pl-set (world-actors w)
                             (obset actor 
                                    'gold (+ (obget o 'gold) 
                                             (obget actor 'gold)))))
      (obset w 'actors (pl-set (world-actors w) actor))))



(define (action-move w act)
  (define from (obget act 'actor-pos))
  (define to (pos-clamp (obget act 'target-pos) 
                        0 (obget w 'width) 0 (obget w 'height)))
  (define actor (pl-get (world-actors w) from))
  (define player-pos (world-player-pos w))

  (cond
    ; should this be an attack instead? (to contains an actor and one of from or to
    ; is the player.)
    [(and (pl-pos? (world-actors w) to) (or (= from player-pos) (= to player-pos)))
     (obset w 'actors (pl-set (world-actors w) 
                              (action-enqueue (action-dequeue actor)
                                              'action-attack
                                              to)))]

    [(and (world-valid-pos? w to) 
          (obhas? (pl-get (world-explored w) to) 'flag-passable))
     (obset 
      w 
      'actors (pl-set 
               (pl-clear (world-actors w) actor)
               (~> (obset actor 'pos to) ; update actor position
                   (action-dequeue)      ; remove look
                   (action-enqueue 'action-pickup to) ; add pickup action
                   (action-enqueue 'action-look to))) ; add look action
      'player-pos (if (= from player-pos) to from))] ;update player pos

    ; else, invalid...remove action.
    [else (obset w 'actors (pl-set (world-actors w) (action-dequeue actor)))]))



(define action-mapping 
  (hash 
   'action-move action-move
   'action-attack action-attack
   'action-pickup action-pickup
   'action-look action-look))

(define (action-do-nothing w act)w)



(define (action-dequeue actor) (obset actor 'actions (rest (obget actor 'actions))))

(define (action-enqueue actor act target)
  (define ao (obset (ob act)
                    'actor-pos (obget actor 'pos)
                    'target-pos target
                    'fn (hash-ref action-mapping act)))

  (obset actor 'actions (append (obget actor 'actions empty) (list ao))))



;
; only the player right now..
;
(define (action-update w)
  (define actions (obget (world-player w) 'actions))
  (if (or (not actions) (null? actions)) w
      (action-update ((obget (first actions) 'fn) w (first actions))))) 

    

