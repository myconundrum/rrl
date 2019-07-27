#lang racket

(require racket/draw "pos.rkt" "object.rkt")
(provide rep->bitmap
         bitmap-size
         string->bitmap)

(define bitmap-size 32)

(define base-path ".//tiles//releases//nov-2015//")

(define (file->bitmap path) 
  (define bm (make-bitmap bitmap-size bitmap-size #t))
  (send bm load-file (string-append base-path path) 'png/alpha)
  bm)



(define bitmap-hash
  (hash
   "." (file->bitmap "dngn//floor//sandstone_floor0.png")
   "#" (file->bitmap "dngn//wall//catacombs0.png")
   "o" (file->bitmap "mon//orc_warrior.png")
   "@" (file->bitmap "player//base//human_m.png")
   "$" (file->bitmap "item//gold//07.png")
   
   "select" (file->bitmap "misc//cursor_green.png")))


(define (rep->bitmap o) (hash-ref bitmap-hash (obget o 'rep)))
(define (string->bitmap s) (hash-ref bitmap-hash s))



