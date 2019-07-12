#lang racket

; world is the set of terrain, entities, items that a player explores.

(require lens unstable/lens racket/random 
         "object.rkt" "pos.rkt" "config.rkt" "dungeon.rkt")

(provide make-world
         
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
         world-player-lens
         world-actors-lens
         world-items-lens
         world-explored-lens
         world-unexplored-lens
         world-width-lens
         world-height-lens
         world-player-pos-lens
         world-player-look-lens)

(define (world-object-at-pos? w t p) (hash-ref (ob-attribute w t) p #f))
(define (world-object w t p) (hash-ref (ob-attribute w t) p #f))

; is a given pos valid in this world?
;(world pos) -> bool
(define (world-valid-pos? w p)
  (let ([x (pos-x p)] [y (pos-y p)])
    (and (>= x 0) (>= y 0) 
         (< x (ob-attribute w "width")) (< y (ob-attribute w "height")))))

; given a list of points, explore these points and return a new world
; (world list) -> world
(define (world-explore w l)
  (lens-set 
   world-explored-lens w
   (foldl (λ (p h) (hash-set h p (ob (if (dungeon-pos-open? 
                                          (ob-attribute w "unexplored") p) 
                                         "floor" "wall")))) 
          (lens-view world-explored-lens w) l)))




(define world-player-lens (ob-make-lens "player"))
(define world-actors-lens (ob-make-lens "actors"))
(define world-items-lens (ob-make-lens "items"))
(define world-explored-lens (ob-make-lens "explored"))
(define world-unexplored-lens (ob-make-lens "unexplored"))
(define world-width-lens (ob-make-lens "width"))
(define world-height-lens (ob-make-lens "height"))
(define world-player-pos-lens (lens-compose ob-pos-lens world-player-lens))
(define world-player-look-lens (lens-compose ob-fov-lens world-player-lens))


(define (gen-treasure d)
  (foldl (λ (n h) (let ([p (dungeon-random-open-pos d)])
                    (hash-set h p (ob "gold" #:pos p)))) (hash) (range 20)))

(define (make-world)
  (define d (create-dungeon columns rows max-rooms))

  (ob "meta" 
      "player" (ob "player" #:pos (dungeon-random-open-pos d))
      "actors" (hash)
      "items" (gen-treasure d)
      "explored" (hash)
      "unexplored" d
      "width" columns
      "height" rows))

