#lang racket

(provide (all-defined-out))

(define (fsplit fnum . splits)
  (rsplit ;;(list
   (file->string
    (string-join (list "./input/" (number->string fnum) ".txt") ""));;)
   splits))

;; each split is either a string delimiter or a transformation procedure string -> list or any
(define (rsplit s splits)
  (cond [(empty? splits) s]
        [(procedure? (car splits))
         (let ((t (apply (car splits) (list s))))
           (cond [(list? t) (map (lambda (subt) (rsplit subt (cdr splits))) t)]
                 [else (rsplit t (cdr splits))]))]
        [(string? (car splits))
         (map (lambda (chunk) (rsplit chunk (cdr splits)))
              (string-split s (car splits)))]))
(define (chunk-list l n)
  (cond [(zero? n) (/ 1 0)]
        [(empty? l) '()]
        [(< (length l) n) (list l)]
        [else (cons (take l n) (chunk-list (list-tail l n) n))]))

(define (vector-set vec pos el)
  (vector-append (vector-take vec pos)
                 (vector-immutable el)
                 (vector-drop vec (add1 pos))))

(define (vector-reverse v)
  (build-vector (vector-length v)
                (lambda (i) (vector-ref v (- (sub1 (vector-length v)) i)))))

(define (vector-modref vec i) (vector-ref vec (modulo i (vector-length vec))))

;; fuck
(define (string->numbers s) (map string->number (filter non-empty-string? (string-split s ""))))

(define (*nz . args) (apply * (filter-not zero? args)))

;; grid

(define (lists->grid lol [cell-proc identity])
  (vector->immutable-vector
   (list->vector
    (map (lambda (r) (vector->immutable-vector
                      (list->vector (map cell-proc r))))
         lol))))

(define (print-grid g [printcell (lambda (c) (print c) (display " "))])
  (for ((r g))
    (for ((c r))
      (printcell c))
    (display "\n"))
  (display "\n"))

(struct pos (x y))

(define (height g) (vector-length g))
(define (width g) (vector-length (vector-ref g 0)))

(define (grid-ref-row g y) (vector-ref g y))
(define (grid-ref-col g x) (vector-map (lambda (r) (vector-ref r x)) g))
(define (grid-ref g y x) (vector-ref (vector-ref g y) x))

(define (grid-set-row g y r) (vector-set g y r))
(define (grid-set-col g x c) (vector-map (lambda (r c) (vector-set r x c)) g c))
(define (grid-set g y x el) (vector-set g y
                                        (vector-set (vector-ref g y) x el)))
;; (define (grid-iter g proc . args)
;; ())

(define (build-grid rows cols [cellproc (lambda (y x) 0)])
  (build-vector rows
                (lambda (r) (build-vector cols
                                          (lambda (c) (cellproc r c))))))

(define (grid-map proc g . gs*)
  (build-vector (height g)
                (lambda (r) (apply vector-map proc
                                   (grid-ref-row g r)
                                   (map (lambda (g) (grid-ref-row g r)) gs*)))))

(define (grid-fold proc ini g)
  (apply foldl proc ini (list (apply append (vector->list (vector-map vector->list g))))))

(define (grid-transpose g)
  (build-grid (width g) (height g) (lambda (y x) (grid-ref g x y))))

(define (grid-rotate g [n 1])
  (cond [(zero? n) g]
        [else (grid-rotate (grid-transpose (vector-reverse g))
                           (sub1 (modulo n 4)))]))

(define (grid-orientations g) (map ((curry grid-rotate) g) '(0 1 2 3)))

;; (define g (lists->grid '((1 2 3) (4 5 6) (7 3 4) ("a" "b" "c"))))
;; (define g2 (lists->grid '((1 0 0) (0 0 0) (0 0 0))))
;; (apply grid-map + (grid-orientations g2))
;; (map print-grid (grid-orientations g))
;; (grid-rotate g)
;; (grid-transpose g)

;; (print-grid (build-grid 5 5 ))
;; (print-grid (build-grid 5 5 (lambda (y x) (* y x))))
;; (grid-ref-col g 0 )
;;
