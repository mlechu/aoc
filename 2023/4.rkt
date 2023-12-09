#lang racket

(require "util.rkt")
(define input (map (λ (card)
                     (let ((w-n (string-split
                                 (string-trim
                                  (second
                                   (string-split card ":"))) " | ")))
                       (list (string-split (first w-n))
                             (string-split (second w-n)))))
                   (fsplit 4 "\n")))
input

(define (matches card)
  (length (set-intersect (first card)
                         (second card))))

(define (p1 input)
  (apply
   + (map (λ (card)
            (match (matches card)
              [0 0]
              [n (expt 2 (- n 1))]))
          input)))

(p1 input)

(define (p2 input)
  (apply + (for/fold ([cardlist (build-list (length input) (λ _ 1))])
                     ([card input]
                      [n (range (length input))])
             (let ((ccount (list-ref cardlist n)))
               (for/fold ([cl cardlist])
                         ([clc (range (add1 n) (+ (add1 n) (matches card)))])
                 (list-update cl clc (curry + ccount)))))))

(println (map matches input))
;; 991 wrong
(println (p2 input))
