#lang racket

(require "util.rkt")
(define input (fsplit 8 "\n" string->numbers))

(define forest (lists->grid input))

(define (each-rotation combine vec-proc)
  (apply grid-map combine
         (map (lambda (onum)
                (print-grid (vector-map vec-proc (grid-rotate forest onum)))
                (grid-rotate (vector-map vec-proc (grid-rotate forest onum))
                             (- 4 onum)))
              '(0 1 2 3))))

(define (vec-visible v)
  (define (visible-r l tallest)
    (cond [(empty? l) '()]
          [(> (car l) tallest) (cons 1 (visible-r (cdr l) (car l)))]
          [else                (cons 0 (visible-r (cdr l) tallest))]))
  (list->vector (visible-r (vector->list v) -inf.0)))

(define (vec-sight v)
  (define (trees-until i n)
    (cond [(< i n) i]
          [(= i n) (add1 n)]
          [(> i n) 0]))
  (define (sight-r l ht)
    (cond [(empty? l) '()]
          [(zero? (apply + ht)) (cons 0 (sight-r (cdr l) (map add1 ht)))]
          [else (cons (apply min (drop ht (car l)))
                      (sight-r (cdr l) (list-set (map add1 ht) (car l) 1)))]))
  (list->vector (sight-r (vector->list v) (build-list 10 (lambda x 0)))))

(define sight-scores (each-rotation * vec-sight))
(define part-2 (grid-fold max 0 sight-scores))
;; (print-grid sight-scores)
part-2
