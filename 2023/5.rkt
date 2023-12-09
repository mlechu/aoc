#lang racket

(require "util.rkt")
(define input (fsplit 5 "\n\n"))
(define input-seeds (map string->number (drop (string-split (first input)) 1)))
(define input-maps
  (map (λ (m) `((,(first (string-split m "-")) ,(third (append-map string-split (string-split m "-"))))
                ,(map (compose (curry map string->number) (curryr string-split " "))
                      (drop (string-split m "\n") 1)))) (drop input 1)))

;; input-seeds
;; input-maps

(define (seed->location input-maps seed)
  (for/fold ([n seed])
            ([i input-maps])
    (let ((found (findf (λ (dsr)
                          (match-let ([(list dst src range) dsr])
                            (and (>= n src) (< n (+ src range))))) (second i))))
      (match found
        [#f n]
        [(list d s r) (+ d (- n s))]))))


(define p1 (apply min (map (curry seed->location input-maps) input-seeds)))
p1

(define input-seeds-2 (chunk-list input-seeds 2))
(define input-maps-2
  (match input-maps
    [`(,@a (,mname ,tlist)) (reverse `(,@a (,mname ,(sort tlist (λ (e1 e2) (< (first e1) (first e2)))))))]))
;; input-seeds-2
;; input-maps-2

(define (location->seed input-maps-2 loc)
  (for/fold ([n loc])
            ([i input-maps-2])
    (let ((found (findf (λ (dsr)
                          (match-let ([(list dst src range) dsr])
                            (and (>= n dst) (< n (+ dst range))))) (second i))))
      (match found
        [#f n]
        [(list d s r) (+ s (- n d))]))))


(define (find-seed-range seeds-2 s)
  (findf (λ (s2) (and (>= s (first s2)) (< s (apply + s2)))) seeds-2))

(define p2 (findf (λ (l) (find-seed-range input-seeds-2 (location->seed input-maps-2 l))) (range 100000000)))

p2
