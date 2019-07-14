#lang racket

; world is the set of terrain, entities, items that a player explores.

(require lens unstable/lens racket/random threading
         "object.rkt" "pos.rkt" "config.rkt" "dungeon.rkt")

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
         world-object-at-pos?
         ; return object at pos.
         world-object
         ; lens accessors below.
         world-actors-lens
         world-items-lens
         world-explored-lens
         world-unexplored-lens
         world-width-lens
         world-height-lens
         ; base accessors
         world-player-pos
         world-player
         world-actors
         world-items
         world-explored
         world-unexplored
         world-time)



(define (world-actors w) (ob-attribute w 'actors))
(define (world-items w) (ob-attribute w 'items))
(define (world-explored w) (ob-attribute w 'explored))
(define (world-unexplored w) (ob-attribute w 'unexplored))
(define (world-time w) (ob-attribute w 'time))
(define (world-player-pos w) (ob-attribute w 'player-pos))
(define (world-player w) (world-object w 'actors (world-player-pos w)))

(define (world-object-at-pos? w t p) (hash-ref (ob-attribute w t) p #f))
(define (world-object w t p) (hash-ref (ob-attribute w t) p #f))

(define (world-update-player w p)
  (ob-attribute-set 
   w
   'player-pos (ob-pos p)
   'actors (hash-set (hash-remove (world-actors w) (world-player-pos w))
                     (ob-pos p) p)))


; is a given pos valid in this world?
;(world pos) -> bool
(define (world-valid-pos? w p)
  (let ([x (pos-x p)] [y (pos-y p)])
    (and (>= x 0) (>= y 0) 
         (< x (ob-attribute w 'width)) (< y (ob-attribute w 'height)))))

; given a list of points, explore these points and return a new world
; (world list) -> world
(define (world-explore w l)
  (lens-set 
   world-explored-lens w
   (foldl (λ (p h) (hash-set h p (ob (if (dungeon-pos-open? 
                                          (ob-attribute w 'unexplored) p) 
                                         'floor 'wall)))) 
          (lens-view world-explored-lens w) l)))




(define world-actors-lens (ob-make-lens 'actors))
(define world-items-lens (ob-make-lens 'items))
(define world-explored-lens (ob-make-lens 'explored))
(define world-unexplored-lens (ob-make-lens 'unexplored))
(define world-width-lens (ob-make-lens 'width))
(define world-height-lens (ob-make-lens 'height))
;(define world-player-pos-lens (lens-compose ob-pos-lens world-player-lens))
;(define world-player-look-lens (lens-compose ob-fov-lens world-player-lens))



(define (world-random-pos w)
  (define p (dungeon-random-open-pos (ob-attribute w 'unexplored)))
  (if (not (or (world-object-at-pos? w 'items p) 
               (world-object-at-pos? w 'actors p)
               (= p (world-player-pos w))))
      p (world-random-pos w)))

(define (gen-treasure w)
  (foldl (λ (n nw)
           (define p (world-random-pos w))
           (ob-attribute-set nw 'items 
                     (hash-set (ob-attribute nw 'items) p
                               (ob 'gold #:pos p)))) w (range 20)))

(define (gen-monsters w)
  (foldl (λ (n nw)
           (define p (world-random-pos w))
           (ob-attribute-set nw 'actors 
                             (hash-set (ob-attribute nw 'actors) p 
                                       (ob 'orc #:pos p)))) w (range 20)))


(define (make-world)

  (define w (ob 'meta 
                'player-pos (pos 0 0)
                'explored (hash)
                'actors (hash)
                'items (hash)
                'unexplored (create-dungeon columns rows max-rooms)
                'width columns
                'height rows
                'time 0))

  (~> w
      (world-update-player (ob 'player #:pos (world-random-pos w)))
      (gen-treasure)
      (gen-monsters)))

