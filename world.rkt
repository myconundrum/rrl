#lang racket

; world is the set of terrain, entities, items that a player explores.

(require racket/random threading "object.rkt" "pos.rkt" "config.rkt" "dungeon.rkt")

(provide make-world
         

         ; Mark list of tiles as explored.
         world-explore

         ; is a given pos valid in this world?
         world-valid-pos?
         ; valid and passable?
         world-passable?


         ; base accessors
         world-player-pos
         world-actors
         world-items
         world-explored
         world-time
         

         world-terrain
         world-actor
         world-item
         world-player
         world-item-set
         world-actor-set
         world-player-set
         world-actor-delete
         world-item-delete
         world-actor-at?
         world-item-at?
         world-player-transform
         world-actor-transform
         world-item-transform
         world-player-attribute
         world-actor-attribute
         world-item-attribute
         world-player-transform-attribute
         world-actor-transform-attribute
         world-item-transform-attribute
         world-player-set-attribute
         world-actor-set-attribute
         world-item-set-attribute
         world-player-delete-attribute
         world-actor-delete-attribute
         world-item-delete-attribute)


(define (world-object-get w l p [def #f]) (hash-ref (hash-ref w l) p def))
(define (world-object-set w l p o) 
  (hash-set w l (hash-set (hash-ref w l) p o)))
(define (world-object-delete w l p) 
  (hash-set w l (hash-remove (hash-ref w l) p)))

(define (world-object-get-attribute w l p k [def #f])
  (obget (world-object-get w l p) k def))

(define (world-object-transform-attribute w l p k tform)
  (define o (world-object-get w l p))
  (define new-o (obset o k (tform o (obget o k))))
  (define new-w (~> (world-object-delete w l p)
                    (world-object-set l (hash-ref new-o 'pos) new-o)))
  (if (= p (hash-ref w 'player-pos)) 
      (hash-set new-w 'player-pos (hash-ref new-o 'pos))
      new-w))

(define (world-object-set-attribute w l p k v)
  (define o (world-object-get w l p))
  (define new-o (obset o k v))
  (define new-w (~> (world-object-delete w l p)
                    (world-object-set l (hash-ref new-o 'pos) new-o)))
  (if (= p (hash-ref w 'player-pos)) 
      (hash-set new-w 'player-pos (hash-ref new-o 'pos))
      new-w))

(define (world-object-delete-attribute w l p k)
  (define o (world-object-get w l p))
  (define new-o (obclear o k))
  (world-object-set l (hash-ref new-o 'pos) new-o))


(define (world-actor-at? w p) (world-object-get w 'actors p))
(define (world-item-at? w p) (world-object-get w 'items p))

(define (world-terrain w p) 
  (list-ref terrain-templates 
            (if (world-object-get w 'terrain p #f) floor-template wall-template)))

(define (world-actor w p [def #f]) (world-object-get w 'actors p def))
(define (world-item w p [def #f]) (world-object-get w 'items p def))
(define (world-player w) (world-object-get w 'actors (hash-ref w 'player-pos)))

(define (world-actor-set w p o) (world-object-set w 'actors p o))
(define (world-item-set w p o) (world-object-set w 'items p o))
(define (world-player-set w o) 
  (~> (hash-set w 'player-pos (hash-ref o 'pos))
      (world-actor-set (hash-ref o 'pos) o)))

(define (world-actor-delete w p) (world-object-delete w 'actors p))
(define (world-item-delete w p) (world-object-delete w 'items p))


(define (world-object-transform w l p tform)
  (define new-o (tform (world-object-get w l p)))
  (define new-w (~> (world-object-delete w l p)
                    (world-object-set l (hash-ref new-o 'pos) new-o)))
  (if (= p (hash-ref w 'player-pos)) 
      (hash-set new-w 'player-pos (hash-ref new-o 'pos))
      new-w))

(define (world-actor-transform-attribute w p k tform) 
  (world-object-transform-attribute w 'actors p k tform))
(define (world-item-transform-attribute w p k tform) 
  (world-object-transform-attribute w 'items p k tform))
(define (world-player-transform-attribute w k tform) 
  (world-object-transform-attribute w 'actors (hash-ref w 'player-pos) k tform))

(define (world-actor-set-attribute w p k v) 
  (world-object-set-attribute w 'actors p k v))
(define (world-item-set-attribute w p k v) 
  (world-object-set-attribute w 'items p k v))
(define (world-player-set-attribute w k v) 
  (world-object-set-attribute w 'actors (hash-ref w 'player-pos) k v))


(define (world-actor-delete-attribute w p k) 
  (world-object-delete-attribute w 'actors p k))
(define (world-item-delete-attribute w p k) 
  (world-object-delete-attribute w 'items p k))
(define (world-player-delete-attribute w k) 
  (world-object-delete-attribute w 'actors (hash-ref w 'player-pos) k))

(define (world-actor-transform w p tform) 
(world-object-transform w 'actors p tform))
(define (world-item-transform w p tform) 
  (world-object-transform w 'items p tform))
(define (world-player-transform w tform) 
  (world-object-transform w 'actors (hash-ref w 'player-pos) tform))

(define (world-actor-attribute w p k [def #f])
  (world-object-get-attribute w 'actors p k def))
(define (world-item-attribute w p k [def #f])
  (world-object-get-attribute w 'items p k def))
(define (world-player-attribute w k [def #f])
  (world-object-get-attribute w 'actors (world-player-pos w) k def))

(define (world-actors w) (hash-ref w 'actors))
(define (world-items w) (hash-ref w 'items))
(define (world-explored w) (hash-ref w 'explored))
(define (world-time w) (hash-ref w 'time))
(define (world-player-pos w) (hash-ref w 'player-pos))


; is a given pos valid in this world?
;(world pos) -> bool
(define (world-valid-pos? w p)
  (let ([x (pos-x p)] [y (pos-y p)])
    (and (>= x 0) (>= y 0) 
         (< x (obget w 'width)) (< y (obget w 'height)))))

(define (world-passable? w p) 
  (and (world-valid-pos? w p) (obhas? (world-terrain w p) 'flag-passable)))


(define (world-explore w l)
  (if (null? l) w
      (world-explore 
       (hash-set w 'explored (hash-set (obget w 'explored) (first l) #t))  (rest l))))


(define (world-random-pos w)
  (define p (random-ref (hash-keys (obget w 'terrain))))
  (if (not (or (world-item w p) (world-actor w p)))
      p (world-random-pos w)))

(define (gen-treasure w)
  (foldl (λ (n nw)
           (define p (world-random-pos nw))
           (world-item-set nw p (ob 'gold #:pos p))) 
         w (range 20)))

(define (gen-monsters w)
  (foldl (λ (n nw)
           (define p (world-random-pos nw))
           (world-actor-set nw p (ob 'orc #:pos p))) 
         w (range 20)))


(define floor-template 0)
(define wall-template 1)
(define terrain-templates (list (ob 'floor) (ob 'wall)))

(define (make-world)
  (define w (ob 'meta 
                'player-pos (pos 0 0)
                'explored (hash)
                'actors (hash)
                'items (hash)
                'terrain (create-dungeon columns rows max-rooms)
                'width columns
                'height rows
                'time 0))

  (~> w
      (world-player-set (ob 'player #:pos (world-random-pos w)))
      (gen-treasure)
      (gen-monsters)))


