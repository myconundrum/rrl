#lang racket

(require "world.rkt" "object.rkt" "pos.rkt" racket/random)

(provide tick-update)

(define (tick-update w)
    (define new-w (obset w 'ticks (add1 (obget w 'ticks 0))))
  

  (if (= (modulo (obget new-w 'ticks) 5) 0) 
      (player-set-attribute new-w 'light-intensity (/ (random 0 3) 30)) 
      new-w)
  
  

)
