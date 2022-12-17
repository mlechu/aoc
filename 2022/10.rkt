#lang racket

(require "util.rkt")
(define input (fsplit 10 "\n" " "))

(define (build-history in x)
  (match in
    [(? empty?) '()]
    [(cons (list "noop") _)
     (cons x (build-history (cdr in) x))]
    [(cons (list "addx" n) _)
     (cons x (cons x (build-history (cdr in) (+ x (string->number n)))))]))

(define history (cons 0 (build-history input 1)))

(define part-1 (foldr + 0 (map (lambda (i)
                                 (* i (list-ref history i)))
                               (build-list 6 (λ (i) (+ 20 (* 40 i)))))))
(build-list 6 (λ (i) (+ 20 (* 40 i))))
(map (lambda (i)
       (* i (list-ref history i)))
     (build-list 6 (λ (i) (+ 20 (* 40 i)))))

history
part-1

;; x: position of sprite is x-1, x, x+1
;; i: cycle - 1
(define part-2-l (map (λ (x i) (if (>= 1 (abs (- i x)))
                                 #\# #\.))
                    (cdr history)
                    (build-list 240 (λ (i) (modulo i 40)))))
(define part-2 (lists->grid (chunk-list part-2-l 40)))
(print-grid part-2 display)
