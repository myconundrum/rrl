#lang racket

(provide ui-handle-input
         ui-draw-world)

(require lux lux/chaos/gui lux/chaos/gui/key racket/draw
         "world.rkt" "config.rkt" "object.rkt" "pos.rkt")



;; (world event) -> world
;;
(define (ui-handle-input w e)
  (cond
    [(key-event? e)
     (case (send e get-key-code)
       [(#\a left) (world-move-player w -1 0)]
       [(#\d right) (world-move-player w 1 0)]
       [(#\s down) (world-move-player w 0 1)]
       [(#\w up) (world-move-player w 0 -1)]
       [(escape #\q) #f]
       [else w])]
    [(eq? 'close w) #f]
    [else w]))


(define (ui-draw-world w width height dc)

  (send dc set-background "black")
  (send dc clear)
  (for ([o (cons (world-player w) (world-terrain w))])
    (send dc set-text-foreground (object-color o))
    (send dc draw-text (object-rep o) 
          (* text-size (pos-x (object-pos o)))
          (* text-size (pos-y (object-pos o))))))
