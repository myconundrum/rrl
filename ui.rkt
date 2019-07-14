#lang racket

(provide ui-handle-input
         ui-draw-world)

(require lens lux lux/chaos/gui lux/chaos/gui/key racket/draw
         "actions.rkt" "world.rkt" "config.rkt" "object.rkt" "pos.rkt" "field.rkt")

(define pixel-pad 3) ; extra padding beyond text-size between windows.
(define msg-width (+ (* 2  pixel-pad) (* 20 text-size)))
(define status-height (+ (* 2 pixel-pad) (* 2 text-size)))
(define ui-bg "Dark Slate Gray")
(define dungeon-bg "black")

(define (ui-fuel-gauge 
         x y width height
         ratio color dc)

  (send dc set-pen color 2 'solid)
  (send dc set-brush color 'transparent)
  (send dc draw-rectangle x y width height)
  (send dc set-brush color 'solid)
  (send dc set-pen color 2 'transparent)
  (send dc draw-rectangle x y (* width ratio) height))

(define (ui-get-string-width s dc)
  (let-values ([(w i1 i2 i3) (send dc get-text-extent s)]) w))

(define (ui-draw-hp w dc)
  (define p (ob-attribute w 'player))
  (define ratio (/ (ob-attribute p 'hp) (ob-attribute p 'maxhp)))
  (define s (format "Hp: ~a / ~a" 
                    (ob-attribute p 'hp)  (ob-attribute p 'maxhp)))

  (define c (cond [(> ratio .9) "green"][(> ratio .4) "yellow"][else "red"]))

  (send dc set-text-foreground c)
  (send dc draw-text s 0 0)
  (ui-fuel-gauge (+ 5 (ui-get-string-width s dc)) 2 
                 (* text-size 10) text-size ratio c dc))

(define (ui-draw-message-panel w width height dc)

  (send dc set-origin (+ (* 2 pixel-pad) (- width msg-width )) pixel-pad)
  (send dc set-text-background ui-bg)
  (send dc set-text-foreground "gold")
  (send dc draw-text (format "Gold: ~a Loc (~a,~a)"
                             (ob-attribute (ob-attribute w 'player) 'gold)
                             (pos-x (lens-view world-player-pos-lens w))
                             (pos-y (lens-view world-player-pos-lens w))) 0 0)

  (send dc set-origin 0 0))

(define (ui-draw-status w width height dc)

  (send dc set-origin pixel-pad (+ (* 2 pixel-pad) (- height status-height)))


  (send dc set-text-background ui-bg)
  (send dc set-text-foreground "white")
  (ui-draw-hp w dc)
 
  (send dc set-origin 0 0))


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


(define (ui-draw-tile x y c a dc)
  (send dc set-alpha a)
  (send dc set-brush c 'solid)
  (send dc draw-rectangle x y text-size text-size)
  (send dc set-alpha 1))


(define (ui-draw-object o p in-fov? dc)
  (define x (* text-size (pos-x p)))
  (define y (* text-size (pos-y p)))
  (send dc set-text-foreground (ob-color o))
  (send dc draw-text (ob-rep o) x y)
  (unless in-fov? (ui-draw-tile x y dungeon-bg .6 dc)))

(define (ui-draw-items w dc)
  (hash-for-each
   (ob-attribute w 'items)
   (λ (p o) (when (field-has-pos? (lens-view world-player-look-lens w) p)
              (ui-draw-object o p #t dc)))))

(define (ui-draw-actors w dc)
  (hash-for-each
   (ob-attribute w 'actors)
   (λ (p o) (when (field-has-pos? (lens-view world-player-look-lens w) p)
              (ui-draw-object o p #t dc)))))

(define (ui-draw-terrain  w dc)
  (hash-for-each 
   (ob-attribute w 'explored) 
   (λ (p o) (ui-draw-object o p (field-has-pos? 
                                 (lens-view world-player-look-lens w) p) dc))))



(define (ui-draw-world  w width height dc)

  ; set standard text mode and clear background.
  (send dc set-background ui-bg)
  (send dc set-text-background dungeon-bg)
  (send dc set-text-mode 'solid)
  (send dc clear)
  
  (send dc set-origin pixel-pad pixel-pad)
  ; order of drawing is important here. Draw terrain then items, then actors
  ; then player.
  (send dc set-brush dungeon-bg 'solid)
  (send dc draw-rectangle 0 0 (- width msg-width) (- height status-height))
  (ui-draw-terrain w dc)
  (ui-draw-items w dc)
  (ui-draw-actors w dc)
  (ui-draw-object (ob-attribute w 'player) 
                  (lens-view world-player-pos-lens w) #t dc )

  (ui-draw-message-panel w width height dc)
  (ui-draw-status w width height dc)
  (send dc set-origin 0 0)
)






