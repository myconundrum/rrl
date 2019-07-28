#lang racket


(provide ui-handle-input)

(require lux lux/chaos/gui lux/chaos/gui/key 
         "actions.rkt" "world.rkt" 
         "object.rkt" "pos.rkt" "field.rkt" )


(define (flip-player-flag w flag) (player-transform w (λ (o) (obflip-flag o flag))))

(define (graphics-mode-toggle w)
  (player-transform-attribute w 'graphics-mode  (λ (o v) (if (eq? 'text v) 'graphics 'text))))



(define (ui-handle-input w e)
  (define p (player-pos w))
  
  (define new-world  
    (cond
      [(key-event? e)
       (case (send e get-key-code)
         [(#\a left) (action-enqueue w 'action-move p (pos-delta p -1 0))]
         [(#\d right) (action-enqueue w 'action-move p (pos-delta p 1 0))]
         [(#\s down)(action-enqueue w 'action-move p (pos-delta p 0 1)) ]
         [(#\w up) (action-enqueue w 'action-move p (pos-delta p 0 -1))]
         [(#\i) (flip-player-flag w 'inventory-mode)]
         [(#\p) (flip-player-flag w 'debug-show-presence)]
         [(#\b) (action-enqueue w 'action-look-at p 0)]
         [(#\m) (graphics-mode-toggle w)]
         [(escape) (action-cancel-modes w)]
         [(#\q) #f]
         [else w])]
      [(eq? 'close w) #f]
      [else w]))
  
  (if new-world (action-update new-world) new-world))
