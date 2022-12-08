#lang racket

(require "util.rkt")
(require racket/set)

(define input (string->list (fsplit 6)))

(define (distinct? l) (= (length l) (set-count (list->set l))))

(define (pstart s curr)
  (cond [(distinct? curr) 0]
        [(empty? s) #f]
        [else (+ 1 (pstart (cdr s) (append (cdr curr) (take s 1))))]))

;; (pstart (string->list "abcd") (#\a #\a #\a #\a))
(define part-1 (+ 4 (pstart (drop input 4) (take input 4))))
part-1
(define part-2 (+ 14 (pstart (drop input 14) (take input 14))))
part-2
