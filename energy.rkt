#lang racket



(require "world.rkt" "object.rkt")


(define base-energy-cost 100)
(define base-speed)

(define (energy-get-next-turn o cur-time)
  (ob-attribute o 'energy-next-turn 
                (+ cur-time (/ base-energy-cost 
                               (ob-attribute o 'energy-speed base-speed)))))



(define (energy-run-actions-worker w next-list prev-list) 
  
  (define o (first next-list))
  (define a (if (ob-attribute o 'actions) (first (ob-attribute o 'actions)) #f))


  
  (if (= (ob-pos o) (ob-pos (world-player w)))
      (if (null? a) )
      
      

  

)

)

(define (energy-run-actions w) 
  (define actor-list
    (sort (cons (world-player w) (hash-values (world-actors w))) < 
          #:key (λ (x) (energy-get-next-turn x (world-time w)))))
  
  


)
