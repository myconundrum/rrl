#lang racket

(require racket/random "object.rkt" "world.rkt" "pos.rkt")

(provide melee-attack
         melee-damage)


(define (d20) (random 1 21))
(define (get-base-damage w p) (random 1 (max (actor-attribute w p 'base-damage 2) 2)))

(define (melee-attack w apos tpos)
  (define roll (d20))
  (cond
    [(= roll 1) #f]
    [(= roll 20) #t]

    ; D20 + strength + hit bonuses > AC + dexterity + ac bonuses
    [else
     (> (+ roll (actor-attribute w apos 'strength 0) (actor-attribute w apos 'hit-bonuses 0))
        (+ (actor-attribute w tpos 'ac) (actor-attribute w tpos 'dexterity 0) 
           (actor-attribute w tpos 'ac-bonuses 0)))]))


(define (melee-damage w apos tpos)(+ (get-base-damage w apos)(actor-attribute w apos 'damage-bonuses 0)))
