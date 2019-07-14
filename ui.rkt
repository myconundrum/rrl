#lang racket

(provide ui-handle-input
         ui-draw-world)

(require lens lux lux/chaos/gui lux/chaos/gui/key racket/draw racket/string
         "actions.rkt" "world.rkt" "config.rkt" "object.rkt" "pos.rkt" "field.rkt")

(define pixel-pad 3) ; extra padding beyond text-size between windows.
(define msg-width (+ (* 2  pixel-pad) (* 20 text-size)))
(define status-height (+ (* 2 pixel-pad) (* 2 text-size)))
(define ui-bg "Dark Slate Gray")
(define dungeon-bg "black")

(define (ui-fuel-gauge 
         dc x y width height
         ratio color)

  (send dc set-pen color 2 'solid)
  (send dc set-brush color 'transparent)
  (send dc draw-rectangle x y width height)
  (send dc set-brush color 'solid)
  (send dc set-pen color 2 'transparent)
  (send dc draw-rectangle x y (* width ratio) height))

(define (ui-get-string-width dc s)
  (let-values ([(w i1 i2 i3) (send dc get-text-extent s)]) w))

(define (ui-draw-hp dc w)
  (define p (world-player w))
  (define ratio (/ (ob-attribute p 'hp) (ob-attribute p 'maxhp)))
  (define s (format "Hp: ~a / ~a" 
                    (ob-attribute p 'hp)  (ob-attribute p 'maxhp)))

  (define c (cond [(> ratio .9) "green"][(> ratio .4) "yellow"][else "red"]))

  (send dc set-text-foreground c)
  (send dc draw-text s 0 0)
  (ui-fuel-gauge dc (+ 5 (ui-get-string-width dc s)) 2 
                 (* text-size 10) text-size ratio c))

(define (ui-draw-message-panel dc w width height)

  (send dc set-origin (+ (* 2 pixel-pad) (- width msg-width )) pixel-pad)
  (send dc set-text-background ui-bg)
  (send dc set-text-foreground "gold")
  (send dc draw-text (format "Gold: ~a Loc (~a,~a)"
                             (ob-attribute (world-player w) 'gold)
                             (pos-x (world-player-pos w))
                             (pos-y (world-player-pos w))) 0 0)

  (send dc draw-text (format "objects in fov: ~a"
                             (ob-attribute (world-player w) 'objects-in-fov)) 0 40
        

)

  (send dc set-origin 0 0))

(define (ui-draw-status dc w width height)

  (send dc set-origin pixel-pad (+ (* 2 pixel-pad) (- height status-height)))


  (send dc set-text-background ui-bg)
  (send dc set-text-foreground "white")
  (ui-draw-hp dc w)
 
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
       [(#\b) (action-player-look-at w)]
       [(escape #\q) #f]
       [else w])]
    [(eq? 'close w) #f]
    [else w]))


(define (ui-draw-tile dc x y c a)
  (send dc set-alpha a)
  (send dc set-brush c 'solid)
  (send dc draw-rectangle x y text-size text-size)
  (send dc set-alpha 1))


(define (ui-draw-object dc o p in-fov?)
  (define x (* text-size (pos-x p)))
  (define y (* text-size (pos-y p)))
  (send dc set-text-foreground (ob-color o))
  (send dc draw-text (ob-rep o) x y)
  (unless in-fov? (ui-draw-tile dc x y dungeon-bg .6)))

(define (ui-draw-items dc w)
  (for ([p (ob-attribute (world-player w) 'objects-in-fov)])
    (define o (ob-attribute (world-items w) p))
    (when o (ui-draw-object dc o p #t))))

(define (ui-draw-actors dc w)
  (for ([p (ob-attribute (world-player w) 'objects-in-fov)])
    (define o (ob-attribute (world-actors w) p))
    (when o (ui-draw-object dc o p #t))))


(define (ui-draw-terrain dc w)
  (hash-for-each 
   (ob-attribute w 'explored) 
   (λ (p o) (ui-draw-object dc o p (field-has-pos?
                                    (ob-attribute (world-player w) 'fov) p)))))

;
; '(<color> <string> ...) 
;

(define (ui-draw-colorful-strings dc x y . sl)
  (unless (null? sl)
    (let ([s (first (rest sl))])
      (send dc set-text-foreground (first sl))
      (send dc draw-text s x y)
      
      (apply ui-draw-colorful-strings dc (+ x (ui-get-string-width dc s)) y 
                                (rest (rest sl))) )))


(define (ui-draw-look-at dc w width height)
  
  (define p (world-player w))
  (define look-pos (if (= -1 (ob-attribute p 'look-at-index)) 
                       #f (list-ref (ob-attribute p 'objects-in-fov) 
                                    (ob-attribute p 'look-at-index))))

  (when look-pos
    (let* ([o (hash-ref (world-actors w) look-pos #f)]
           [o (if o o (hash-ref (world-items w) look-pos #f))])
      ; BUGBUG - Should clean up the +1 , +5 skew below to line up
      ; the view target.
      (ui-draw-tile dc (+ 1 (* (pos-x look-pos) text-size)) 
                    (+ 5 (* (pos-y look-pos) text-size)) "red" .5)

      
      ; BUGBUG - Also a bad look here. this draw text works to put
      ; into the status window only because I know the layout here. 
      ; should be separated out, but seems wasteful. Refactor?
      (send dc set-text-background ui-bg)
      (ui-draw-colorful-strings 
       dc 300 (- height text-size text-size) 
       "white" "You see " (ob-color o) (ob-attribute o 'look-desc) "white" ".")
      (send dc set-text-background dungeon-bg))))




(define (ui-draw-world dc w width height)

  (define p (world-player w))


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
  (ui-draw-terrain dc w)
  (ui-draw-items dc w)
  (ui-draw-actors dc w)
  (ui-draw-object dc p (ob-pos p) #t)

  
  
  (ui-draw-message-panel dc w width height)
  (ui-draw-status dc w width height)
  
  (ui-draw-look-at dc w width height)
 
  (send dc set-origin 0 0))






