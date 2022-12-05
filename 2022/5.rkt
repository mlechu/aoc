#lang racket

(require "util.rkt")

;; top -> bottom
(define c1-init (list #\C #\Q #\B))
(define c2-init (list #\Z #\W #\Q #\R ))
(define c3-init (list #\V #\L #\R #\M #\B ))
(define c4-init (list #\W #\T #\V #\H #\Z #\C))
(define c5-init (list #\G #\V #\N #\B #\H #\Z #\D))
(define c6-init (list #\Q #\V #\F #\J #\C #\P #\N #\H))
(define c7-init (list #\S #\Z #\W #\R #\T #\G #\D))
(define c8-init (list #\P #\Z #\W #\B #\N #\M #\G #\C ))
(define c9-init (list #\P #\F #\Q #\W #\M #\B #\J #\N))

(define stacks (list '() c1-init c2-init c3-init c4-init c5-init c6-init c7-init c8-init c9-init))

(define input0 (fsplit 5 "\n\n" "\n" " from "))
(define input (map (lambda (i) (list (string->number (substring (first i) 5))
                                     (string->number (substring (second i) 0 1))
                                     (string->number (list-ref (string-split (second i) " to ") 1))))
                   (list-ref input0 1)))

input
;;
;; ins = (count, from, to)
;;

(define (set-list l idx val)
  ;; (println l)
  ;; (println idx)
  ;; (println val)
  ;; (println "===")
  (cond [(zero? idx) (cons val (cdr l))]
        [else (cons (car l) (set-list (cdr l) (- idx 1) val))]))

(define (ct ins) (first ins))
(define (from ins) (second ins))
(define (to ins) (third ins))

(define (move-s stks ins)
  ;; (println stks)
  ;; (println ins)
  (cond [(zero? (ct ins)) stks]
        [else (move-s (set-list (set-list stks (to ins) (cons (first (list-ref stks (from ins))) (list-ref stks (to ins))))
                                (from ins)
                                (cdr (list-ref stks (from ins))))
                      (list (sub1 (ct ins)) (from ins) (to ins)))]))

(define (first-or-nil l) (if (zero? (length l)) null (first l)))
(define part-1 (map first-or-nil (foldl (lambda (ins stks) (move-s stks ins)) stacks input)))
part-1

(define (move-many stks ins)
  (set-list (set-list stks (to ins) (append (take (list-ref stks (from ins)) (ct ins)) (list-ref stks (to ins))))
            (from ins)
            (drop (list-ref stks (from ins)) (ct ins))))

(define part-2 (map first-or-nil (foldl (lambda (ins stks) (move-many stks ins)) stacks input)))
part-2
