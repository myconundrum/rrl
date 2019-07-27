#lang racket

(provide ui-handle-input ui-render-all)

(require lux lux/chaos/gui lux/chaos/gui/key racket/draw racket/string
         "actions.rkt" "world.rkt" "config.rkt" "gfx.rkt"
         "object.rkt" "pos.rkt" "field.rkt" "msg.rkt")

(define ui-bg "Dark Slate Gray")
(define dungeon-bg "black")

(define (debug-presence-view w)
  (world-player-transform w (λ (o) (obflip-flag o 'debug-show-presence))))

(define (graphics-mode-toggle w)
  (world-player-transform-attribute w 'graphics-mode  (λ (o v) (if (eq? 'text v) 'graphics 'text))))

(define (ui-handle-input w e)
  (define p (world-player-pos w))
  
  (define new-world  
    (cond
      [(key-event? e)
       (case (send e get-key-code)
         [(#\a left) (action-enqueue w 'action-move p (pos-delta p -1 0))]
         [(#\d right) (action-enqueue w 'action-move p (pos-delta p 1 0))]
         [(#\s down)(action-enqueue w 'action-move p (pos-delta p 0 1)) ]
         [(#\w up) (action-enqueue w 'action-move p (pos-delta p 0 -1))]
         [(#\p) (debug-presence-view w)]
         [(#\b) (action-enqueue w 'action-look-at p 0)]
         [(#\m) (graphics-mode-toggle w)]
         [(escape) (action-cancel-modes w)]
         [(#\q) #f]
         [else w])]
      [(eq? 'close w) #f]
      [else w]))
  
  (if new-world (action-update new-world) new-world))

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
  (camera (pos-delta (world-player-pos w) (- (quotient width 2)) (- (quotient height 2))) 
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
  (define p (world-player w))
  (if (= -1 (obget p 'look-at-index)) #f (list-ref (obget p 'objects-in-fov) 
                                                   (min (obget p 'look-at-index)   
                                                        (sub1 (length (obget p 'objects-in-fov)))))))


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
(define (draw-object dc ctx cam o p in-fov?)
  (define p2 (camera-pos cam p))
  (when (and (>= (pos-x p2) 0) (>= (pos-y p2) 0) 
             (< (pos-x p2) (camera-width cam)) (< (pos-y p2) (camera-height cam)))
    (cond 
      [(eq? (context-mode ctx) 'text)
       (send dc set-text-foreground (obget o 'color))
       (send dc draw-text (obget o 'rep) 
             (* (pos-x p2) (context-base-size ctx)) (* (pos-y p2) (context-base-size ctx)))]
      [else (send dc draw-bitmap (rep->bitmap o) 
                  (* (pos-x p2) (context-base-size ctx)) (* (pos-y p2) (context-base-size ctx)))]) 
    (unless in-fov? (draw-tile dc ctx cam p dungeon-bg .6))))


; draw highlight over looked-at object.
(define (draw-look-at dc w ctx cam)
  (define look-pos (get-look-at-pos w))
  (when look-pos (draw-tile dc ctx cam look-pos "red" .5)))

; draw presence field.
(define (draw-presence dc w ctx cam)
  (when (and (obhas? (world-player w) 'debug-show-presence) (obhas? (world-player w) 'presence))
    (hash-for-each (obget (world-player-attribute w 'presence) 'points)
                   (λ (p w) (draw-tile dc ctx cam p "red" (max 0.1 w))))))


(define (draw-items dc w ctx cam)
  (for ([p (obget (world-player w) 'objects-in-fov)])
    (define o (obget (world-items w) p))
    (when o (draw-object dc ctx cam o p #t))))

(define (draw-actors dc w ctx cam)
  (for ([p (obget (world-player w) 'objects-in-fov)])
    (define o (obget (world-actors w) p))
    (when o (draw-object dc ctx cam o p #t))))

(define (draw-terrain dc w ctx cam)
  (hash-for-each (obget w 'explored) 
                 (λ (p v) (draw-object dc ctx cam (world-terrain w p) p 
                                       (field-has-pos? (obget (world-player w) 'fov) p)))))


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
                             (world-player-attribute w 'gold)
                             (pos-x (world-player-pos w))
                             (pos-y (world-player-pos w))
                            (quotient (world-time w) 100)) 0 0)
  
  (draw-message-list dc 0 50 (context-width ctx) (get-messages w)))

(define (draw-fuel-gauge dc x y width height ratio color)
  (send dc set-pen color 2 'solid)
  (send dc set-brush color 'transparent)
  (send dc draw-rectangle x y width height)
  (send dc set-brush color 'solid)
  (send dc set-pen color 2 'transparent)
  (send dc draw-rectangle x y (* width ratio) height))


(define (draw-hp dc w ctx)
  (define p (world-player w))
  (define ratio (/ (obget p 'hp) (obget p 'maxhp)))
  (define s (format "Hp: ~a / ~a" 
                    (obget p 'hp)  (obget p 'maxhp)))

  (define c (cond [(> ratio .9) "green"][(> ratio .4) "yellow"][else "red"]))

  (send dc set-text-foreground c)
  (send dc draw-text s 0 0)
  (draw-fuel-gauge dc (+ 5 (get-string-width dc s)) 2 
                 (* text-size 10) text-size ratio c))

(define (draw-look-at-message dc w ctx)
  (define look-pos (get-look-at-pos w))

  (when look-pos
    (let* ([o (world-actor w look-pos)]
           [o (if o o (world-item w look-pos))])
      (draw-colorful-strings 
       dc text-size (- (context-height ctx) text-size text-size) 
       "white" "You see " (obget o 'color) (obget o 'look-desc) "white" "."))))


(define (draw-status dc w ctx)
  (set-origin-to-context dc ctx)
  (clear-ui-view dc w ctx)
  (draw-hp dc w ctx)
  (draw-look-at-message dc w ctx)
)

(define (draw-game-view dc w width height)
  (define mode (world-player-attribute w 'graphics-mode 'graphics))
  (define base-size (get-base-size mode))
  (define game-ctx (context mode base-size (pos 0 0) (floor (* width .8)) (floor (* height .9))))

  (define messages-ctx (context mode base-size 
                                (pos-delta (context-origin game-ctx) (context-width game-ctx) 0) 
                                (- width (context-width game-ctx)) (context-height game-ctx)))
  (define status-ctx (context mode base-size 
                              (pos-delta (context-origin game-ctx) 0 (context-height game-ctx)) 
                              width (- height (context-height game-ctx))))
  
  (clear-screen dc)
  (draw-game dc w game-ctx)
  (draw-messages dc w messages-ctx)
  (draw-status dc w status-ctx)
  
)

(define views 
  (hash 'game draw-game-view))


(define (ui-render-all dc w width height)
  ((hash-ref views 'game) dc w width height))
