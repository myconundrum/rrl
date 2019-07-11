#lang racket

(provide ui-handle-input
         ui-draw-world)

(require lens lux lux/chaos/gui lux/chaos/gui/key racket/draw
         "actions.rkt" "world.rkt" "config.rkt" "object.rkt" "pos.rkt" "field.rkt")


;; (world event) -> world
;;
(define (ui-handle-input w e)
  (cond
    [(key-event? e)
     (case (send e get-key-code)
       [(#\a left) (action-player-move w -1 0)]
       [(#\d right) (action-player-move w 1 0)]
       [(#\s down) (action-player-move  w 0 1)]
       [(#\w up) (action-player-move w 0 -1)]
       [(#\l) (action-player-look w)]
       [(escape #\q) #f]
       [else w])]
    [(eq? 'close w) #f]
    [else w]))


(define (ui-draw-fov w dc)

  (send dc set-alpha .5)
  (send dc set-brush "yellow" 'solid)

  (for ([p (hash-keys (field-points (lens-view player-look-lens w)))])
    (send dc draw-rectangle
            (* text-size (pos-x p))
            (* text-size (pos-y p))
            text-size
            text-size))
  (send dc set-alpha 1)
 )


(define (ui-draw-object o dc)
  
  (send dc set-text-foreground (object-color o))
  (send dc draw-text (object-rep o) 
        (* text-size (pos-x (object-pos o)))
        (* text-size (pos-y (object-pos o)))))


(define (ui-draw-terrain  w dc)

  (hash-for-each (world-terrain w) 
                 (λ (p o)
                   (send dc set-text-foreground (object-color o))
                   (send dc draw-text (object-rep o) 
                         (* text-size (pos-x p))
                         (* text-size (pos-y p))))))


(define (ui-draw-world  w width height dc)

  (send dc set-background "black")
  (send dc clear)

  (ui-draw-terrain w dc)
  (ui-draw-object (world-player w) dc)
  (ui-draw-fov w dc))





