#lang racket

; world is the set of terrain, entities, items that a player explores.

(require lens unstable/lens racket/random threading
         "object.rkt" "pos.rkt" "config.rkt" "dungeon.rkt" "poslist.rkt")

(provide make-world
         
         ; update player state in world
         world-update-player

         ; given a list of points (from a field of view of some sort typically)
         ; explore those points from the unexplored terrain turning them into
         ; real explored tiles
         world-explore
         ; is a given pos valid in this world?
         world-valid-pos?
         ; check if an object is in the given object list.


         ; base accessors
         world-player-pos
         world-player
         world-actors
         world-items
         world-explored
         world-unexplored
         world-time)



(define (world-actors w) (obget w 'actors))
(define (world-items w) (obget w 'items))
(define (world-explored w) (obget w 'explored))
(define (world-unexplored w) (obget w 'unexplored))
(define (world-time w) (obget w 'time))
(define (world-player-pos w) (obget w 'player-pos))
(define (world-player w) (pl-get (world-actors w) (world-player-pos w)))


(define (world-update-player w p)
   (obset w 'player-pos (obget p 'pos)
          'actors (pl-set (pl-clear (world-actors w) (world-player-pos w)) p)))


; is a given pos valid in this world?
;(world pos) -> bool
(define (world-valid-pos? w p)
  (let ([x (pos-x p)] [y (pos-y p)])
    (and (>= x 0) (>= y 0) 
         (< x (obget w 'width)) (< y (obget w 'height)))))

; given a list of points, explore these points and return a new world
; (world list) -> world
(define (world-explore w l)
  (obset w 'explored (foldl (λ (p pl) 
                              (pl-set pl 
                                      (ob (if (dungeon-pos-open? 
                                               (obget w 'unexplored) p) 
                                              'floor 'wall) #:pos p)))  
                            (world-explored w) l)))


(define (world-random-pos w)
  (define p (dungeon-random-open-pos (obget w 'unexplored)))
  (if (not (or (pl-pos? (world-items w) p) (pl-pos? (world-actors w) p)))
      p (world-random-pos w)))

(define (gen-treasure w)
  (foldl (λ (n nw)
           (define p (world-random-pos nw))
           (obset nw 'items
                  (pl-set (world-items nw) (ob 'gold #:pos p)))) 
         w (range 20)))

(define (gen-monsters w)
  (foldl (λ (n nw)
           (define p (world-random-pos nw))
           (obset nw 'actors
                  (pl-set (world-actors nw) (ob 'orc #:pos p)))) 
         w (range 20)))

(define (make-world)
  (define w (ob 'meta 
                'player-pos (pos 0 0)
                'explored (make-pl)
                'actors (make-pl)
                'items (make-pl)
                'unexplored (create-dungeon columns rows max-rooms)
                'width columns
                'height rows
                'time 0))

  (~> w
      (world-update-player (ob 'player #:pos (world-random-pos w)))
      (gen-treasure)
      (gen-monsters)))

