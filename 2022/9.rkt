#lang racket

(require "util.rkt")
(define input (map (lambda (p)
                     (list (string-ref (first p) 0)
                           (string->number (second p))))
                   (fsplit 9 "\n" " ")))

input
(define dirs '(#\U #\R #\D #\L))

(define (move x y dir)
  (match dir
    [#\U (list x (sub1 y))]
    [#\R (list (add1 x) y)]
    [#\D (list x (add1 y))]
    [#\L (list (sub1 x) y)]))

(define (/a n) (/ n (abs n)))

(define (follow x y tx ty)
  (let ([movement (cond
                   [(>= 1 (abs (*nz (- x tx) (- y ty)))) (list 0 0)]
                   [(= x tx) (list 0             (/a (- y ty)))]
                   [(= y ty) (list (/a (- x tx)) 0            )]
                   [else     (list (/a (- x tx)) (/a (- y ty)))])])
    (map + movement (list tx ty))))

(define (dec-insl insl)
  (cons (list (first (car insl)) (sub1 (second (car insl))))
        (cdr insl)))

;; grid is a hash table (pos, count)

(define (step hx hy tx ty insl grid)
  (cond [(empty? insl) grid]
        [(zero? (second (car insl))) (step hx hy tx ty (cdr insl) grid)]
        [else (let* ([nexth (move hx hy (first (car insl)))]
                     [nextt (follow (first nexth) (second nexth) tx ty)])
                (apply step (append nexth nextt (list (dec-insl insl) (hash-update grid nextt add1 0)))))]))

(define part-1 (hash-count (step 0 0 0 0 input (hash))))
part-1

(define (follow-l head knots)
  (cond [(empty? knots) '()]
        [else (let ([nextt (follow (first head) (second head)
                                   (first (car knots)) (second (car knots)))])
                (cons nextt (follow-l nextt (cdr knots))))]))

(define (step2 head knots insl grid)
  (cond [(empty? insl) grid]
        [(zero? (second (car insl))) (step2 head knots (cdr insl) grid)]
        [else (let* ([nexth (move (first head) (second head) (first (car insl)))]
                    [nextts (follow-l nexth knots)])
                (step2 nexth nextts (dec-insl insl) (hash-update grid (last nextts) add1 0)))]))


(define part-2 (hash-count (step2 (list 0 0)
                                  (build-list 9 (lambda (_) (list 0 0)))
                                  input
                                  (hash))))
part-2
