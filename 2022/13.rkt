#lang racket

(require "util.rkt")
(define input
  (fsplit 13 "\n\n" "\n"
          (curryr string-replace "," " ")
          (compose read open-input-string)))

;; #f keep going
;; (#t . #t) true
;; (#t . #f) false
(define (ordered p)
  (match p
    [(list a a) #f]
    [(or `(,a ()) `(() ,a)) (cons #t (empty? (first p)))]
    [(list (? number? a) (? number? b)) (cons #t (< a b))]
    [(list (? number? a) (cons b r)) (ordered `((,a) ,(second p)))]
    [(list (cons a r) (? number? b)) (ordered `(,(first p) (,b)))]
    [(list (cons a r1) (cons b r2)) (or (ordered (list a b)) (ordered (list r1 r2)))]))

(define part-1 (foldl (λ (p i sum) (+ sum (if (cdr (ordered p)) i 0)))
                      0
                      input (build-list (length input) add1)))
part-1

(define dividers '(((2)) ((6))))
(define sorted (sort (flatten1 (cons dividers input)) (compose cdr ordered list)))
(define part-2 (foldl * 1 (map (λ (d) (add1 (index-of sorted d))) dividers)))

part-2
