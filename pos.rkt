#lang racket

(provide (struct-out rect)
         pos
         pos-x
         pos-y
         pos-delta
         pos-clamp
         rect-center
         rects-intersect?)



; using complex numbers as our representation of positions.
; (nat nat) -> (pos)
; x is stored as the real part and y as the imaginary.
(define (pos x y) (make-rectangular x y))

; (pos) -> nat
(define (pos-x p) (real-part p))
(define (pos-y p) (imag-part p))

; (pos nat nat) -> pos
(define (pos-delta p dx dy) (pos (+ (pos-x p) dx) (+ (pos-y p) dy)))


;(pos nat nat nat nat) -> pos
(define (pos-clamp p minx maxx miny maxy)
  (pos
   (min (max (pos-x p) minx) maxx)
   (min (max (pos-y p) miny) maxy)))

; a rect is just a pair of positions. 
; (pos pos) -> rect
(struct rect (p1 p2) #:transparent)


; midpoint of two positions or center of a rect.
; (rect) -> pos
(define (rect-center r)
  (let ([mp (/ (+ (rect-p1 r) (rect-p2 r)) 2 )]) 
    (pos (floor (pos-x mp)) (floor (pos-y mp)))))

; (rect rect) -> bool
(define (rects-intersect? r1 r2)
  (and (<= (pos-y (rect-p1 r1)) (pos-y (rect-p2 r2)))
       (>= (pos-y (rect-p2 r1)) (pos-y (rect-p1 r2)))
       (<= (pos-x (rect-p1 r1)) (pos-x (rect-p2 r2)))
       (>= (pos-x (rect-p2 r1)) (pos-x (rect-p1 r2)))))
