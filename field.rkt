#lang racket


;
; field of view functions. Cast rays and return field of points.
;


(provide field-has-pos?
         field-get-pos
         field-get-points
         make-field
         field-points-lens
         field-radius-lens)

(require lens "pos.rkt" "object.rkt")

;  Draw a line from x1,y1 to x2,y2 using Bresenham's. 
; Adapted from http://rosettacode.org/wiki/Bitmap/Bresenham's_line_algorithm#Clojure
; ported to racket
; simplified using multiple-rebinding in let and redesigned to return 
; points in original order from x1 y1 -> x2 y2 


; (pos pos) -> '(pos ...) 
(define (points-in-line p1 p2)

  ; don't judge me.
  (let* ([origin p1] ; remember origin to reverse points if necessary.
         [steep (> (abs (- (pos-y p1) (pos-y p2))) (abs (- (pos-x p1) (pos-x p2))))]
         [p1 (if steep (pos-swapxy p1) p1)] ;swap x and y components if steep.
         [p2 (if steep (pos-swapxy p2) p2)]
         [temp p1] ; swap if necessary so that p2 > p1
         [p1 (if (> (pos-x p1) (pos-x p2)) p2 p1)]
         [p2 (if (= p1 temp) p2 temp)]
         [dx (- (pos-x p2) (pos-x p1))]  ; distance to travel
         [dy (abs (- (pos-y p2) (pos-y p1)))]
         [step (if (< (pos-y p1) (pos-y p2)) 1 -1)] ; step in y direction
         [addpoint (if steep 
                       (λ (l p) (append l (list (pos-swapxy p))))
                       (λ (l p) (append l (list p))))])

    ;
    ; now that all setup is complete, run bresenham's algorithm.
    ;
    (let loop ([coords empty] [p p1]  [error (quotient dx 2)])
      (if (> (pos-x p) (pos-x p2)) 
          (if (= (first coords) origin) coords (reverse coords)) ; end.
          (if (< error dy)
              (loop (addpoint coords p) (pos-delta p 1 step) (+ error (- dx dy)))  
              (loop (addpoint coords p) (pos-delta p 1 0) (- error dy)))))))



; (pos nat nat) -> '(pos ...)
(define (mirror-point p dx dy) 
  (list (pos-delta p dx dy)
        (pos-delta p dy dx)
        (pos-delta p (- dy) dx)
        (pos-delta p (- dx) dy)
        (pos-delta p (- dx) (- dy))
        (pos-delta p (- dy) (- dx))
        (pos-delta p dy (- dx))
        (pos-delta p dx (- dy))))


; https://en.wikipedia.org/wiki/Midpoint_circle_algorithm
; adapted and ported from:
; https://gist.github.com/devstopfix/b397424e9b39396ca0abaf6b3b2c0707#file-midpoint-circle-algorithm-clj
; Return a seq of (pos) of a circle of given radius centered on pos

; (pos nat) -> (pos ...)
(define (circle-points p0 radius)

  (let loop ([x radius] [y 0] [err 0] [result empty])
    (if (>= x y)
        (let ([p (mirror-point p0 x y)]
              [y (add1 y)] [err (+ err (add1 (* 2 y)))])
          
          (if (positive? (add1 (* 2 (- err x)))) 
              (loop (sub1 x) y (+ err (- 1 (* 2 x))) (append result p))
              (loop x y err (append result p))))
        (remove-duplicates result))))

;  "Returns a set of rays for the given field."
; (pos nat) -> (pos ...)
(define (get-field-rays p0 radius)
  (map (λ (p1)  (points-in-line p0 p1)) (circle-points p0 radius)))

;(pos pos) -> real
(define (weight-point p0 p1)
  (let ([diff (- p1 p0)])
    (/ 1 (sqrt (+ 1 (expt (pos-x diff) 2) (expt (pos-y diff) 2))))))

;(pos '(pos ...)) -> #hash of (pos . weight)
(define (weight-points p0 points)
  (make-hash (map (λ (p1) (cons p1 (weight-point p0 p1))) points)))

;(field pos) -> bool
(define (field-has-pos? f p) (hash-ref (ob-attribute f "points") p #f))
;(field pos) -> pos
(define (field-get-pos f p) (hash-ref (ob-attribute f "points") p #f))

(define (field-get-points f) (hash-keys (ob-attribute f "points")))


; ('(pos ...) blocks-rayfn '(pos ...)) -> '(pos ...))
(define (until-blocked points blocks-ray? [return-points empty])
  (if (or (null? points) (blocks-ray? (first points)))
      (if (null? points) return-points  (append return-points (take points 1)))
      (until-blocked (rest points) blocks-ray? (append return-points (take points 1)))))

; (pos nat blocks-rayfn) -> field
(define (cast-field origin radius blocks-ray?)
  (flatten (map (λ (ray) (until-blocked ray blocks-ray?)) (get-field-rays origin radius))))


(define field-points-lens (ob-make-lens "points"))
(define field-radius-lens (ob-make-lens "radius"))



; (pos nat fn that returns true if a pos blocks a ray) -> (pos ...)
(define (make-field origin radius blocks-ray?) 
  (ob "meta" #:pos origin "radius" radius
      "points" (weight-points origin (cast-field origin radius blocks-ray?))))
