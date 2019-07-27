#lang racket

(require racket/draw "pos.rkt" "object.rkt")
(provide rep->bitmap
         bitmap-size)

(define bitmap-size 32)

(define (file->bitmap path) 
  (define bm (make-bitmap bitmap-size bitmap-size #t))
  (send bm load-file path 'png/alpha)
  bm)

(define bitmap-hash
  (hash
   "." (file->bitmap "//users//marcw//downloads//dungeon crawl stone soup full//dungeon//floor//sandstone_floor_0.png")
   "#" (file->bitmap "//Users//marcw//Downloads//Dungeon Crawl Stone Soup Full//dungeon//wall//catacombs_0.png")
   "o" (file->bitmap "//Users//marcw//Downloads//Dungeon Crawl Stone Soup Full//monster//orc_warrior_new.png")
   "@" (file->bitmap "//Users//marcw//Downloads//Dungeon Crawl Stone Soup Full//player//base//human_male.png")
   "$" (file->bitmap "//Users//marcw//Downloads//Dungeon Crawl Stone Soup Full//item//gold//gold_pile_7.png"


                                        )


   



)

)
(define (rep->bitmap o) (hash-ref bitmap-hash (obget o 'rep)))




