#lang racket

(provide ui-render-all)

(require lux lux/chaos/gui lux/chaos/gui/key racket/draw racket/string
         "actions.rkt" "world.rkt" "config.rkt" "gfx.rkt"
         "object.rkt" "pos.rkt" "field.rkt" "msg.rkt")

(define ui-bg "Dark Slate Gray")
(define dungeon-bg "black")


;; STRUCTURES

; context struct is used to define a particular frame in the ui.
;
; : mode - 'text 'graphics
; : base-size - size of one element (font size in text, bitmap size in graphics
; : camera - skew to ensure player is visible
; : width - current width of overall screen
; : height - curren theight of overall height
;
(struct context (mode base-size origin width height))

; camera struct is used to transform map points into a screen position.
; : off - offset to subtract from a given position
; : width - width in base-size units for the camera view.
; : height - height in base-size units for the camera view. 

(struct camera (off width height))
(define (make-camera w ctx)
  (define width (quotient (context-width ctx) (context-base-size ctx)))
  (define height (quotient (context-height ctx) (context-base-size ctx)))
  (camera (pos-delta (player-pos w) (- (quotient width 2)) (- (quotient height 2))) 
          width height))

;; UTILITY FUNCTIONS

; get all messages
(define (get-messages w)
  (map (λ (i) (list-ref (obget w 'messages) (modulo (+ i (obget w 'message-index))
                                                    (obget w 'message-count)))) 
       (range (obget w 'message-count))))

; return the correct base size for the graphics mode
(define (get-base-size mode) (if (eq? mode 'text) text-size bitmap-size))

; return the UI width of a given string. 
(define (get-string-width dc s)
  (let-values ([(w i1 i2 i3) (send dc get-text-extent s)]) w))

; clear the entire screen.
(define (clear-screen dc)

  ; set standard text mode and clear background.
  (send dc set-background dungeon-bg)
  (send dc set-text-background dungeon-bg)
  (send dc set-text-mode 'solid)
  (send dc clear))

; clear a context sized section.
(define (clear-ui-view dc w ctx)
  (send dc set-brush ui-bg 'solid)
  (send dc draw-rectangle 0 0 (context-width ctx) (context-height ctx))
  (send dc set-brush ui-bg 'transparent)
  (send dc set-text-background ui-bg))

; set the origin in a given context.
(define (set-origin-to-context dc ctx) 
  (send dc set-origin (pos-x (context-origin ctx)) (pos-y (context-origin ctx))))

; translate a map point to a position.
(define (camera-pos c p) 
  (pos (- (pos-x p) (pos-x (camera-off c))) (- (pos-y p) (pos-y (camera-off c)))))

; get a position being looked at by the player if it exists.
(define (get-look-at-pos w)
  (define p (player w))
  (if (= -1 (obget p 'look-at-index)) #f (list-ref (obget p 'objects-in-fov) 
                                                   (min (obget p 'look-at-index)   
                                                        (sub1 (length (obget p 'objects-in-fov)))))))

(define (hp-ratio o) (/ (obget o 'hp) (obget o 'maxhp)))

(define (hp-gauge-color o)
  (define ratio (hp-ratio o))
  (cond [(> ratio .75) "green"][(> ratio .4) "yellow"][else "red"]))


; turn a msg into a set of colored strings.
(define (msg->colored-strings m [l (list "white")])
  (define r (regexp-match-positions #rx"([{][a-z]*[}])" m))
  (if (not r) (append l (list m))
      (let ([c (substring m (add1 (car (first r))) (sub1 (cdr (first r))))]
            [b (substring m 0 (car (first r)))])
        (msg->colored-strings 
         (substring m (cdr (first r)))    
         (append l (list b) (list c))))))

;; DRAWING FUNCTIONS

(define (has-inventory-item w s)
  (> (length (filter (λ (o) (string=? (obget o 'rep) s)) (player-attribute w 'inventory empty))) 0))

(define (draw-object-bitmap dc w ctx o x y)
  (cond 
    [(= (player-pos w) (obget o 'pos))
     (send dc draw-bitmap (rep->bitmap o) x y)
     ; BUGBUG: dumb right now...hard coded, does nothing.
     (when (has-inventory-item w "[") 
       (send dc draw-bitmap (string->bitmap "player armor") x y))
     (when (has-inventory-item w "|") 
       (send dc draw-bitmap (string->bitmap "player sword") x y))]
    [(obhas? o 'flag-actor) 
     (when (< (obget o 'hp) (obget o 'maxhp))
       (draw-fuel-line dc x (+  y bitmap-size) bitmap-size (hp-ratio o) (hp-gauge-color o)))
     (send dc draw-bitmap (rep->bitmap o) x y) ]
    [else (send dc draw-bitmap (rep->bitmap o) x y)]))

; draw a rectangle of base-size with the given color and alpha.
(define (draw-tile dc ctx cam p c a)
  (define p2 (camera-pos cam p))
  (define sz (context-base-size ctx))
  (send dc set-alpha a)
  (send dc set-brush c 'solid)
  (send dc draw-rectangle 
        (* (context-base-size ctx) (pos-x p2)) (* (context-base-size ctx) (pos-y p2)) sz sz)
  (send dc set-alpha 1))


; draw one game object. Use in-fov? flag to draw alpha if necessary.
(define (draw-object dc w ctx cam o p in-fov?)
  (define p2 (camera-pos cam p))
  (when (and (>= (pos-x p2) 0) (>= (pos-y p2) 0) 
             (< (pos-x p2) (camera-width cam)) (< (pos-y p2) (camera-height cam)))
    (define x (* (pos-x p2) (context-base-size ctx))) 
    (define y (* (pos-y p2) (context-base-size ctx))) 
    (cond 
      [(eq? (context-mode ctx) 'text)
       (send dc set-text-foreground (obget o 'color))
       (send dc draw-text (obget o 'rep) x y)]
      [else (draw-object-bitmap dc w ctx o x y)]) 
    
    (draw-tile dc ctx cam p dungeon-bg 
               (if in-fov? (max 0 (- .8 (player-attribute w 'light-intensity)
                                     (field-get-pos (player-attribute w 'fov) p))) .8 ))))


(define (draw-select-box dc w ctx cam p)
  (define p2 (camera-pos cam p))
  (send dc draw-bitmap (string->bitmap "select") 
        (* (pos-x p2) (context-base-size ctx)) (* (pos-y p2) (context-base-size ctx))))

; draw highlight over looked-at object.
(define (draw-look-at dc w ctx cam)
  (define look-pos (get-look-at-pos w))
  (when look-pos 
    (if (eq? 'text (context-mode ctx)) (draw-tile dc ctx cam look-pos "red" .5)
        (draw-select-box dc w ctx cam look-pos))))

; draw presence field.
(define (draw-presence dc w ctx cam)
  (when (and (obhas? (player w) 'debug-show-presence) (obhas? (player w) 'presence))
    (hash-for-each (obget (player-attribute w 'presence) 'points)
                   (λ (p w) (draw-tile dc ctx cam p "red" (max 0.1 w))))))

(define (draw-items dc w ctx cam)
  (for ([p (obget (player w) 'objects-in-fov)])
    (define o (obget (items w) p))
    (when o (draw-object dc w ctx cam o p #t))))

(define (draw-actors dc w ctx cam)
  (for ([p (obget (player w) 'objects-in-fov)])
    (define o (obget (actors w) p))
    (when o (draw-object dc w ctx cam o p #t))))

(define (draw-terrain dc w ctx cam)
  (hash-for-each (obget w 'explored) 
                 (λ (p v) (draw-object dc w ctx cam (terrain w p) p 
                                       (field-has-pos? (obget (player w) 'fov) p)))))

(define (draw-game dc w ctx)
  (define cam (make-camera w ctx))
  (set-origin-to-context dc ctx)
  (draw-terrain dc w ctx cam)
  (draw-items dc w ctx cam)
  (draw-actors dc w ctx cam)
  (draw-look-at dc w ctx cam)
  (draw-presence dc w ctx cam))

; draw a string with wrapping.
(define (draw-string-wrapped dc x y sx ex str-list)
  (if (null? str-list) (pos x y) 
      (let* ([s (format "~a " (first str-list))]
             [w (get-string-width dc s)]
             [y2 (if (> (+ w x) ex) (+ text-size y) y)]
             [x2 (if (> (+ w x) ex) sx x)])
        
        (send dc draw-text s x2 y2)
        (draw-string-wrapped dc (+ x2 w) y2 sx ex (rest str-list)))))

(define (draw-each-colored-string dc x y minx maxx cs)
  (if (null? cs) y
      (let ([split-str (string-split (first (rest cs)))])
        (send dc set-text-foreground (first cs))
        (let ([p (draw-string-wrapped dc x y minx maxx split-str)]) 
          (draw-each-colored-string 
           dc (pos-x p) (pos-y p) minx maxx (rest (rest cs)))))))

(define (draw-colorful-strings dc x y . sl)
  (unless (null? sl)
    (let ([s (first (rest sl))])
      (send dc set-text-foreground (first sl))
      (send dc draw-text s x y)
      
      (apply draw-colorful-strings dc (+ x (get-string-width dc s)) y 
                                (rest (rest sl))) )))

(define (draw-msg dc x y maxx m)
  (draw-each-colored-string dc x y x maxx (msg->colored-strings m)))

(define (draw-message-list dc x y maxx msgs)
  (unless (null? msgs)
    (let ([y2 (draw-msg dc x y maxx (first msgs))])
      (draw-message-list dc x (+ text-size 3 y2) maxx (rest msgs)))))

(define (draw-messages dc w ctx)
  (set-origin-to-context dc ctx)
  (clear-ui-view dc w ctx)
  
  (send dc set-text-foreground "gold")
  (send dc draw-text (format "Gold: ~a Loc (~a,~a) Turn (~a)"
                             (player-attribute w 'gold)
                             (pos-x (player-pos w))
                             (pos-y (player-pos w))
                            (quotient (game-time w) 100)) 0 0)
  
  (cond 
    [(player-attribute w 'inventory-mode) #t]
    [else (draw-message-list dc 0 50 (context-width ctx) (get-messages w)) ]))

(define (draw-fuel-gauge dc x y width height ratio color)
  (send dc set-pen color 2 'solid)
  (send dc set-brush color 'transparent)
  (send dc draw-rectangle x y width height)
  (send dc set-brush color 'solid)
  (send dc set-pen color 2 'transparent)
  (send dc draw-rectangle x y (* width ratio) height))

(define (draw-fuel-line dc x y width ratio color)
  (send dc set-pen color 2 'solid)
  (send dc draw-line x y (+ x (* width ratio)) y) 
  (send dc set-pen color 2 'transparent))


(define (draw-hp dc w ctx)
  (define p (player w))
  (define s (format "Hp: ~a / ~a" (obget p 'hp)  (obget p 'maxhp)))
  (define c (hp-gauge-color p))

  (send dc set-text-foreground c)
  (send dc draw-text s 0 0)
  (draw-fuel-gauge dc (+ 5 (get-string-width dc s)) 2 
                   (* text-size 10) text-size (hp-ratio p) c))

(define (draw-look-at-message dc w ctx)
  (define look-pos (get-look-at-pos w))
  (when look-pos
    (let* ([o (actor w look-pos)]
           [o (if o o (item w look-pos))])
      (draw-colorful-strings dc 300 0 "white" 
                             "You see " (obget o 'color) (obget o 'look-desc) "white" "."))))
(define (draw-status dc w ctx)
  (set-origin-to-context dc ctx)
  (clear-ui-view dc w ctx)
  (draw-hp dc w ctx)
  (draw-look-at-message dc w ctx))

(define status-height bitmap-size)
(define message-width (* 8 bitmap-size))

(define (draw-game-view dc w width height)
  (define mode (player-attribute w 'graphics-mode 'graphics))
  (define base-size (get-base-size mode))
  (define game-ctx (context mode base-size (pos 0 0) (- width message-width) (- height status-height)))
  (define messages-ctx (context mode base-size 
                                (pos-delta (context-origin game-ctx) (context-width game-ctx) 0) 
                                (- width (context-width game-ctx)) (context-height game-ctx)))
  (define status-ctx (context mode base-size 
                              (pos-delta (context-origin game-ctx) 0 (context-height game-ctx)) 
                              width (* 2 bitmap-size)))
  (clear-screen dc)
  (draw-game dc w game-ctx)
  (draw-messages dc w messages-ctx)
  (draw-status dc w status-ctx))

(define views 
  (hash 'game draw-game-view))

(define (ui-render-all dc w width height)
  ((hash-ref views 'game) dc w width height))
