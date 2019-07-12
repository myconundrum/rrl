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


(define (ui-draw-fov w width height dc)

  (let* ([radius (* text-size (field-radius (lens-view player-look-lens w)))]
         [diameter (* 2 radius)]
         [x (-  (* text-size (pos-x (lens-view player-pos-lens w))) (-  radius (quotient  text-size 2)))]
         [y (-  (* text-size (pos-y (lens-view player-pos-lens w))) (-  radius (quotient text-size 2)))]

         [p-fov (new dc-path%)]
         [p-world (new dc-path%)]
         [r-fov (new region%)]
         [r-final (new region%)])

    
    (send dc set-brush "black" 'solid)
    (send p-fov ellipse x y diameter diameter)
    (send r-fov set-path p-fov)
    (send p-world rectangle 0 0 width height)
    (send r-final set-path p-world)
    (send r-final subtract r-fov)
    (send dc set-clipping-region r-final)
    (send dc set-alpha .6)
    (send dc draw-rectangle 0 0 width height)
    (send dc set-alpha 1)
    (send dc set-clipping-region #f)))



(define (ui-shade-tile x y dc)

  (send dc set-alpha .6)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle x y text-size text-size)
  (send dc set-alpha 1))


(define (ui-draw-object o p in-fov? dc)
  (define x (* text-size (pos-x p)))
  (define y (* text-size (pos-y p)))

  (send dc set-text-foreground (object-color o))
  (send dc draw-text (object-rep o) x y)
  (unless in-fov? (ui-shade-tile x y dc)))



(define (ui-draw-terrain  w dc)
  (hash-for-each 
   (world-terrain w) 
   (λ (p o) (ui-draw-object o p (field-has-pos? (lens-view player-look-lens w) p) dc))))


(define (ui-draw-status w dc)
  (send dc set-text-foreground "white")
  (send dc draw-text (format "(~a,~a)" 
                             (pos-x (lens-view player-pos-lens w))
                             (pos-y (lens-view player-pos-lens w))) 0 0))

(define (ui-draw-world  w width height dc)

  (send dc set-background "black")
  (send dc clear)

  (ui-draw-terrain w dc)
  (ui-draw-object (world-player w) (lens-view player-pos-lens w) #t dc )
  (ui-draw-status w dc))





