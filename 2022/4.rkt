#lang racket

(require "./util.rkt")

(define input (fsplit 4 "\n" "," "-"))

(define (a-in-b a b)
  (and (>= (string->number (first a)) (string->number (first b)))
       (<= (string->number (second a)) (string->number (second b)))))

(define (bad-pair p)
  (or (a-in-b (first p) (second p))
      (a-in-b (second p) (first p))))
;; input
(define part-1 (length (filter bad-pair input)))
part-1

(define (bad-pair-2 p)
  (or (and (>= (string->number(first  (second p))) (string->number(first (first p))))
           (<= (string->number(first  (second p))) (string->number(second (first p)))))
      (and (>= (string->number(first  (first p))) (string->number(first (second p))))
           (<= (string->number(first  (first p))) (string->number(second (second p)))))))
(define part-2 (length (filter bad-pair-2 input)))
part-2
