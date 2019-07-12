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


(define (ui-shade-tile x y dc)

  (send dc set-alpha .6)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle x y text-size text-size)
  (send dc set-alpha 1))


(define (ui-draw-object o p in-fov? dc)
  (define x (* text-size (pos-x p)))
  (define y (* text-size (pos-y p)))

  (send dc set-text-foreground (ob-color o))
  (send dc draw-text (ob-rep o) x y)
  (unless in-fov? (ui-shade-tile x y dc)))



(define (ui-draw-terrain  w dc)
  (hash-for-each 
   (ob-attribute w "explored") 
   (λ (p o) (ui-draw-object o p (field-has-pos? 
                                 (lens-view world-player-look-lens w) p) dc))))


(define (ui-draw-status w dc)
  (send dc set-text-foreground "white")
  (send dc draw-text (format "(~a,~a)" 
                             (pos-x (lens-view world-player-pos-lens w))
                             (pos-y (lens-view world-player-pos-lens w))) 0 0))

(define (ui-draw-world  w width height dc)

  (send dc set-background "black")
  (send dc clear)

  (ui-draw-terrain w dc)
  (ui-draw-object (ob-attribute w "player") 
                  (lens-view world-player-pos-lens w) #t dc )
  (ui-draw-status w dc))





