#lang racket 

(require threading gregor "object.rkt" "world.rkt")
(provide msg 
         msg-queue
         msg-log
         msg-init
         msg-strip-colors
         msg-cleanup)

(define log-file (open-output-file "log.txt" #:exists 'replace))
(define msg-queue-size 20)

(define (msg-init w) (obset w 
                            'messages (map (λ (x) (msg w 0 0  ""))  (range msg-queue-size))
                            'message-index 0
                            'message-count msg-queue-size))

(define (msg-cleanup w) 
  (close-output-port log-file))

(define (time-stamp-msg s) (format "[~a]: ~a" (~t (now) "[E d MMM YYYY h:mm:ss a]") s))
(define (attribute->string ob attr) (format "~a" (obget ob (string->symbol attr) "")))
(define (get-color w actor target color)
  (format "{~a}" (cond 
                   [(string-ci=? color "actor") (obget actor 'color)]
                   [(string-ci=? color "target") (obget target 'color)]
                   [else color])))

(define (do-strip s l) 
  (string-append (substring s 0 (car l)) (substring s (add1 (cdr l)))))
(define (msg-strip-colors s)
  (define m (regexp-match-positions #rx"([{][a-zA-Z-]*[}])" s))
  (if (not m) s (msg-strip-colors (do-strip s (first m)))))



(define (do-substitution w actor target s l)
  (define keyword (substring s (car l) (cdr l)))
  (define arg2 (first (string-split (substring s (cdr l)))))
  (define arg  (first (regexp-match #rx"^[a-zA-Z-]*" (substring s (cdr l)))))
  (define before (substring s 0 (car l)))
  (define after (substring s (+ (cdr l) (string-length arg))))

  (string-append before 
                 (cond 
                   [(string-ci=? keyword "%color:") (get-color w actor target arg)]
                   [(string-ci=? keyword "%actor:") (attribute->string actor arg)]
                   [(string-ci=? keyword "%target:") (attribute->string target arg)])
                 after))

(define (msg-substitutions w actor target s)
  (define m (regexp-match-positions #rx"([%][a-zA-Z-]*[:])" s))
  (if (not m) s
      (msg-substitutions w actor target (do-substitution w actor target s (first m)))))

(define (msg w actor target str . kv)
  (msg-substitutions w actor target (string-append (apply format str kv) "\n")))

(define (msg-log m)
  (define m2 (msg-strip-colors m))
  (printf (msg-strip-colors m))
  (fprintf log-file (time-stamp-msg m2)))

(define (msg-queue w m)
  (let-values ([(b a) (split-at (obget w 'messages) (modulo (obget w 'message-index)
                                                            (obget w 'message-count)))])
    (msg-log m)
    (obset w 
           'messages (append b (cons m (rest a)))
           'message-index (add1 (obget w 'message-index)))))

