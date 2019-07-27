#lang racket

; world is the set of terrain, entities, items that a player explores.
(require racket/random threading "object.rkt" "pos.rkt" "config.rkt" "dungeon.rkt")

(provide make-world
         ; Mark list of tiles as explored.
         explore
         ; is a given pos valid in this world?
         valid-pos?
         ; valid and passable?
         passable?
         ; base accessors
         player-pos
         actors
         items
         explored
         game-time
         terrain
         actor
         item
         player
         item-set
         actor-set
         player-set
         actor-delete
         item-delete
         actor-at?
         item-at?
         player-transform
         actor-transform
         item-transform
         player-attribute
         actor-attribute
         item-attribute
         player-transform-attribute
         actor-transform-attribute
         item-transform-attribute
         player-set-attribute
         actor-set-attribute
         item-set-attribute
         player-delete-attribute
         actor-delete-attribute
         item-delete-attribute)


(define (object-get w l p [def #f]) (hash-ref (hash-ref w l) p def))
(define (object-set w l p o) (hash-set w l (hash-set (hash-ref w l) p o)))
(define (object-delete w l p) (hash-set w l (hash-remove (hash-ref w l) p)))
(define (object-get-attribute w l p k [def #f]) (obget (object-get w l p) k def))
(define (object-transform-attribute w l p k tform)
  (define o (object-get w l p))
  (define new-o (obset o k (tform o (obget o k))))
  (define new-w (~> (object-delete w l p) (object-set l (hash-ref new-o 'pos) new-o)))
  (if (= p (hash-ref w 'player-pos)) 
      (hash-set new-w 'player-pos (hash-ref new-o 'pos))
      new-w))
(define (object-set-attribute w l p k v)
  (define o (object-get w l p))
  (define new-o (obset o k v))
  (define new-w (~> (object-delete w l p) (object-set l (hash-ref new-o 'pos) new-o)))
  (if (= p (hash-ref w 'player-pos)) 
      (hash-set new-w 'player-pos (hash-ref new-o 'pos))
      new-w))
(define (object-delete-attribute w l p k)
  (define o (object-get w l p))
  (define new-o (obclear o k))
  (object-set l (hash-ref new-o 'pos) new-o))
(define (actor-at? w p) (object-get w 'actors p))
(define (item-at? w p) (object-get w 'items p))
(define (terrain w p) 
  (list-ref terrain-templates (if (object-get w 'terrain p #f) floor-template wall-template)))
(define (actor w p [def #f]) (object-get w 'actors p def))
(define (item w p [def #f]) (object-get w 'items p def))
(define (player w) (object-get w 'actors (hash-ref w 'player-pos)))
(define (actor-set w p o) (object-set w 'actors p o))
(define (item-set w p o) (object-set w 'items p o))
(define (player-set w o) 
  (~> (hash-set w 'player-pos (hash-ref o 'pos))
      (actor-set (hash-ref o 'pos) o)))
(define (actor-delete w p) (object-delete w 'actors p))
(define (item-delete w p) (object-delete w 'items p))
(define (object-transform w l p tform)
  (define new-o (tform (object-get w l p)))
  (define new-w (~> (object-delete w l p) (object-set l (hash-ref new-o 'pos) new-o)))
  (if (= p (hash-ref w 'player-pos)) (hash-set new-w 'player-pos (hash-ref new-o 'pos)) new-w))
(define (actor-transform-attribute w p k tform) (object-transform-attribute w 'actors p k tform))
(define (item-transform-attribute w p k tform) (object-transform-attribute w 'items p k tform))
(define (player-transform-attribute w k tform) 
  (object-transform-attribute w 'actors (hash-ref w 'player-pos) k tform))
(define (actor-set-attribute w p k v) (object-set-attribute w 'actors p k v))
(define (item-set-attribute w p k v) (object-set-attribute w 'items p k v))
(define (player-set-attribute w k v) (object-set-attribute w 'actors (hash-ref w 'player-pos) k v))
(define (actor-delete-attribute w p k) (object-delete-attribute w 'actors p k))
(define (item-delete-attribute w p k) (object-delete-attribute w 'items p k))
(define (player-delete-attribute w k) (object-delete-attribute w 'actors (hash-ref w 'player-pos) k))
(define (actor-transform w p tform) (object-transform w 'actors p tform))
(define (item-transform w p tform) (object-transform w 'items p tform))
(define (player-transform w tform) (object-transform w 'actors (hash-ref w 'player-pos) tform))
(define (actor-attribute w p k [def #f]) (object-get-attribute w 'actors p k def))
(define (item-attribute w p k [def #f])(object-get-attribute w 'items p k def))
(define (player-attribute w k [def #f]) (object-get-attribute w 'actors (player-pos w) k def))
(define (actors w) (hash-ref w 'actors))
(define (items w) (hash-ref w 'items))
(define (explored w) (hash-ref w 'explored))
(define (game-time w) (hash-ref w 'time))
(define (player-pos w) (hash-ref w 'player-pos))
(define (valid-pos? w p)
  (let ([x (pos-x p)] [y (pos-y p)]) 
    (and (>= x 0) (>= y 0) (< x (obget w 'width)) (< y (obget w 'height)))))
(define (passable? w p) (and (valid-pos? w p) (obhas? (terrain w p) 'flag-passable)))
(define (explore w l)
  (if (null? l) w (explore (hash-set w 'explored (hash-set (obget w 'explored) (first l) #t))(rest l))))
(define (random-pos w)
  (define p (random-ref (hash-keys (obget w 'terrain))))
  (if (not (or (item w p) (actor w p))) p (random-pos w)))
(define (gen-treasure w)
  (foldl (λ (n nw)
           (define p (random-pos nw))
           (item-set nw p (ob 'gold #:pos p))) w (range 20)))
(define (gen-monsters w)
  (foldl (λ (n nw)
           (define p (random-pos nw))
           (actor-set nw p (ob 'orc #:pos p))) w (range 20)))

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
  (~> w (player-set (ob 'player #:pos (random-pos w))) (gen-treasure) (gen-monsters)))


