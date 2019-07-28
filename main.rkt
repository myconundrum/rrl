#lang racket

(require lux lux/chaos/gui "actions.rkt" "ui.rkt" "world.rkt" "msg.rkt" "tick.rkt" )

;; game-loop is the structure interfacing with lux to
;; drive the game state machine.
(struct game-loop (world)

  #:methods gen:word
  [
   (define (word-fps w) 30.0)
   (define (word-label w framerate) "Racket RL")
   (define (word-event w e) 
     (let ([nw (ui-handle-input (game-loop-world w) e)])
       (if nw (game-loop nw) nw)))
   
   (define (word-output w)
     (λ (width height dc)
       (ui-render-all dc (game-loop-world w) width height)))

   (define (word-tick w) (game-loop (tick-update (game-loop-world w))))])

(define (main)
  (define w (msg-init (make-world)))
  (call-with-chaos 
   (make-gui) (λ () (fiat-lux (game-loop (action-player-look w)))))
  (msg-cleanup w))

(main)
