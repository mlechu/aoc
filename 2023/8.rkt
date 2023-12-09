#lang racket

(require "util.rkt")
(define input (match (fsplit 8 "\n\n")
                [`(,lrs ,nodes) `(,(string->list lrs)
                                  ,(map (compose (curryr string-split " ")
                                                 (curryr string-replace "= (" "")
                                                 (curryr string-replace "," "")
                                                 (curryr string-replace ")" ""))
                                        (string-split nodes "\n")))]))
input

(define c-rmap (foldl (λ (node ht) (hash-set ht (first node) (third node))) (hash) (second input)))
(define c-lmap (foldl (λ (node ht) (hash-set ht (first node) (second node))) (hash) (second input)))

(define (steps-to-end input start-node)
  (for/fold ([steps 0]
             [curr start-node])
            ([dir (build-list (* 100 (length (first input))) ; yeah
                              (λ (x) (list-ref (first input)
                                               (modulo x (length (first input))))))])
    (match (list dir curr)
      [(list _ s) #:when (equal? #\Z (string-ref s 2))
                  (values steps curr)]
      [(list #\L _) (values (add1 steps) (hash-ref c-lmap curr))]
      [(list #\R _) (values (add1 steps) (hash-ref c-rmap curr))])))

(define-values (p1 _) (steps-to-end input "AAA"))
p1

(define ends-a (filter (λ (x) (equal? #\A (string-ref x 2))) (hash-keys c-rmap)))
(define ea-steps (map (λ (ea) (define-values (a b) (steps-to-end input ea)) a) ends-a))

(define p2 (apply lcm ea-steps))
p2
