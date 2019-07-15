#lang racket



(require "world.rkt" "object.rkt")


(define base-energy-cost 100)
(define base-speed)

(define (get-next-turn o cur-time)
  (ob-attribute o 'energy-next-turn 
                (+ cur-time (/ base-energy-cost 
                               (ob-attribute o 'energy-speed base-speed)))))





(define (next-actor w)
  (define cur-time (world-time w))
  (define actors (pl-all-objects (world-actors w)))

  (foldl (λ (o min-o) (if (< (get-next-turn o cur-time) 
                             (get-next-turn min-o cur-time)) o min-o))
         (first (actors)) (rest (actors))))

(define (take-action w a)


)

(define (energy-run-actions w) 
  
  (define a (next-actor w))
  (if (and (= (ob-pos a) (world-player-pos w)) (not (ob-attribute 'action))) 
      ; blocked on player input.
      w
      ; take current action and look for next action.
      (energy-run-actions (take-action w a))))
