#lang racket

(require "util.rkt")
(define input (fsplit 2 "\n" ":" ";" "," string-trim))
input


(define (p1 input)
  (define p1-max
    `(("red" . 12)
      ("green" . 13)
      ("blue" . 14)))

  (define (ok-draw d)
    (foldl (λ (n-c acc)
             ;; (print (string-split n-c))
             (and acc (match (string-split n-c)
                        [`(,n "red") (<= (string->number n) 12)]
                        [`(,n "green") (<= (string->number n) 13)]
                        [`(,n "blue") (<= (string->number n) 14)]
                        [_ #f])))
           #t d))

  (apply + (map (λ (game)
                  (match (foldl (λ (draw acc) (and acc (ok-draw draw))) #t (second game))
                    [#t (string->number (second (string-split (caaar game))))]
                    [#f 0])) input)))

(p1 input)

(define (p2 input)
  (println "game")
  (foldl
   (λ (ds acc)
     (define r-hash
       (let ((flat (apply append ds)))
         (foldl (λ (n-c ht)
                  (match-let ([`(,n ,c) (string-split n-c)])
                    (hash-update ht c
                                 (λ (old) (max old (string->number n)))
                                 (string->number n)))) (hash) flat)))
     (+ acc
        (* (hash-ref r-hash "red" 1)
           (hash-ref r-hash "green" 1)
           (hash-ref r-hash "blue" 1)))
     )
   0 (map second input)))

(p2 input)
