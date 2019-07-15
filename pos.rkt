#lang racket

; position (x, y) utilities.

(provide (struct-out rect)
         pos
         pos?
         x-lens
         y-lens
         pos-x
         pos-y
         pos-delta
         pos-clamp
         pos-swapxy
         rect-center
         rects-intersect?)

(require lens)

(define x-lens
    (make-lens real-part
               (λ (n r) (make-rectangular (real-part r) (imag-part n)))))
(define y-lens
    (make-lens imag-part
               (λ (n i) (make-rectangular (real-part n) (real-part i)))))


; using complex numbers as our representation of positions.
; (nat nat) -> (pos)
; x is stored as the real part and y as the imaginary.
(define (pos x y) (make-rectangular x y))

; (pos) -> nat
(define (pos-x p) (real-part p))
(define (pos-y p) (imag-part p))

; (pos) -> bool
(define (pos? p) (complex? p))

; (pos nat nat) -> pos
(define (pos-delta p dx dy) (pos (+ (pos-x p) dx) (+ (pos-y p) dy)))

; (pos) -> (pos)
(define (pos-swapxy p) (pos (pos-y p) (pos-x p)))

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
