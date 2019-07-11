#lang racket

(require racket/random racket/hash "pos.rkt" "config.rkt")
(provide create-dungeon)


(define min-room-size 4)
(define max-room-size 12)
(define room-size-range (- max-room-size min-room-size))

;() -> nat
(define (random-wall-length) (random min-room-size max-room-size))
;(nat nat) -> pos
(define (random-pos width height) 
  (pos (random 1 (- width 1)) (random 1 (- height 1))))

;() -> bool
(define (coin-toss) (random-ref (list #t #f)))

;(room (room ...)) -> bool
(define (room-intersects-rooms? room rooms)
  (not (null? (filter (λ (r) (rects-intersect? room r)) rooms))))

; (nat nat) -> rect
(define (random-rect width height)
  (let ([p1 (random-pos width height)])
    (rect p1 (pos-delta p1 (random-wall-length) (random-wall-length)))))

; (nat nat nat) -> (room ...)
(define (create-rooms width height max-rooms)
  (foldl 
   (λ (n rooms) 
     (let ([room (random-rect width height)])
       (if (room-intersects-rooms? room rooms) rooms (cons room rooms))))
   empty (range max-rooms)))

;(rect (object ...)) -> (object ...)
(define (dig-one-room room [terrain empty])
  (flatten 
   (map (λ (x) (map (λ (y) (pos x y)) 
                    (range (pos-y (rect-p1 room)) (pos-y (rect-p2 room)))))
        (range (pos-x (rect-p1 room)) (pos-x (rect-p2 room))))))

; ((rect ...) (object ...)) -> (object ...)
(define (dig-rooms rooms [terrain empty])
  (if (null? rooms) terrain
      (dig-rooms (rest rooms) (append (dig-one-room (first rooms)) terrain))))

; (nat nat nat) -> (object ...)
(define (dig-horizontal-tunnel x1 x2 y) 
  (let ([xmin (min x1 x2)])
    (map (λ (x) (pos (+ xmin  x) y) ) (range (abs (- x1 x2))))))
; (nat nat nat) -> (object ...)
(define (dig-vertical-tunnel y1 y2 x) 
  (let ([ymin (min y1 y2)])
    (map (λ (y) (pos x (+ ymin  y))) (range (abs (- y1 y2))))))

; (pos pos) -> (object ...)
(define (dig-one-tunnel p1 p2)
  (append (dig-horizontal-tunnel (pos-x p1) (pos-x p2) (pos-y p1))
          (dig-vertical-tunnel (pos-y p1) (pos-y p2) (pos-x p2))))

; ((rect ...) (object ...)) -> (object ...)
(define (dig-tunnels rooms [terrain empty])
  (if (< (length rooms) 2)
      terrain
      (dig-tunnels (rest rooms) 
                   (append (dig-one-tunnel 
                            (rect-center (first rooms)) 
                            (rect-center (second rooms))) 
                           terrain))))

;(nat nat nat) -> #hash of pos
(define (create-dungeon width height maxrooms)
  (let ([rooms (create-rooms width height maxrooms)])
    (make-hash 
     (map (λ (p) (cons p #t)) (append (dig-tunnels rooms) (dig-rooms rooms))))))
