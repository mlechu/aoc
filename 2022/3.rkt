#lang racket

(require "./util.rkt")

(define input1
  (map (lambda (rs)
         (list (substring rs 0 (/ (string-length rs) 2))
               (substring rs (/ (string-length rs) 2))))
       (fsplit 3 "\n")))

(define common
  (for/list ([i input1])
    (set-intersect (string->list (first i))
                   (string->list (second i)))))
;; common

(define (char-value c)
  (cond ;;[(set-member? (range (char->integer #\A)
    ;; (+ 1 (char->integer #\A))) c)]
    [(char-lower-case? c) (+ 1  (- (char->integer c) (char->integer #\a)))]
    [(char-upper-case? c) (+ 27 (- (char->integer c) (char->integer #\A)))]))

(define part-1  (apply + (map char-value (flatten common))))

part-1

(define input2 (chunk-list (fsplit 3 "\n") 3))

(define common2
  (for/list ([i input2])
    (set-intersect (set-intersect (string->list (first i))
                                  (string->list (second i)))
                   (string->list (third i)))))

(define part-2 (apply + (map char-value (flatten common2))))

part-2
