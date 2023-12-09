#lang racket

(require "util.rkt")
(define input (fsplit 9 "\n" " " string->number))
;; input

(define (difflist l)
  (map (λ (x) (- (list-ref l (add1 x))
                 (list-ref l x)))
       (range (sub1 (length l)))))

;; lst -> (values steps newval)
(define (predict-f l)
  (match l
    [(list 0 ...) 0]
    [_ (+ (last l) (predict-f (difflist l)))]))

(map difflist input)


(define p1 (apply + (map predict-f input)))

(define (predict-b l)
  (match l
    [(list 0 ...) 0]
    [_ (- (first l) (predict-b (difflist l)))]))

(define p2 (apply + (map predict-b input)))
(map predict-b input)
p1
p2
