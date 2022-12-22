#lang racket

(require "util.rkt")
(define input (lists->grid (fsplit 12 "\n" string->list char->integer)))
;; (print-grid input)

(define start-n (char->integer #\S))
(define end-n (char->integer #\E))

(struct pos (x y z path))
(define unvisited #f)

(define (ppos p)
  (displayln (list "pos: " (pos-x p)
                   "," (pos-y p)
                   "," (pos-z p)
                   " path: " (pos-path p))))

(define (find-v g el)
  (index-where (vector->list g)
               (λ (v)
                 (not (vector-empty?
                       (vector-filter (curry = el)
                                      v))))))
(define startpos
  (pos (find-v (grid-transpose input) start-n)
       (find-v input start-n)
       (char->integer #\a) unvisited))

(define endpos
  (pos (find-v (grid-transpose input) end-n)
       (find-v input end-n)
       (char->integer #\z) unvisited))

(define workgrid
  (grid-set
   (grid-set
    (build-grid (height input) (width input)
                (λ (y x) (pos x y (grid-ref input y x) unvisited)))
    (pos-y startpos)
    (pos-x startpos)
    startpos)
   (pos-y endpos)
   (pos-x endpos)
   endpos))

(define (neighbours p g)
  (map (λ (ib) (struct-copy pos ib (z (grid-ref input (pos-y ib) (pos-x ib)))))
       (filter (λ (n) (and (>= (pos-x n) 0) (>= (pos-y n) 0)
                           (< (pos-x n) (width g)) (< (pos-y n) (height g))))
               (list (pos (add1 (pos-x p)) (pos-y p) -1 (cons #\r (pos-path p)))
                     (pos (sub1 (pos-x p)) (pos-y p) -1 (cons #\l (pos-path p)))
                     (pos (pos-x p) (add1 (pos-y p)) -1 (cons #\d (pos-path p)))
                     (pos (pos-x p) (sub1 (pos-y p)) -1 (cons #\u (pos-path p)))))))

(define (shorter-path? newp oldp)
  (or (equal? unvisited (pos-path oldp))
      (> (length (pos-path oldp)) (length (pos-path newp)))))

(define (okn p g)
  (filter
   (λ (n)
     (and (<= (sub1 (pos-z n))
              (pos-z (grid-ref g
                               (pos-y p)
                               (pos-x p))))
          (shorter-path? n (grid-ref
                            g
                            (pos-y n)
                            (pos-x n)))))

   (neighbours p g)))

(define (fill-grid-c p wg)
  (let ((ip (struct-copy pos p (path '()))))
    (fill-grid (list ip) (grid-set wg (pos-y p) (pos-x p) ip))))

(define (fill-grid q wg)
  (cond [(empty? q) wg]
        [else (let* ((p (first q))
                     (ok (okn p wg)))
                (fill-grid (cdr (append q ok))
                           ;; (grid-set wg (pos-y p) (pos-x p) p)
                           (foldl (λ (k g)
                                    (grid-set g (pos-y k) (pos-x k) k)) wg ok)))]))

(define part-1 (grid-ref (fill-grid-c startpos workgrid) (pos-y endpos) (pos-x endpos)))
(println (length (pos-path part-1)))

;; wait a bit. going for efficiency in code size.
(define part-2 (grid-fold (λ (c m) (let ((endpath (pos-path (grid-ref (fill-grid-c c workgrid)
                                                                       (pos-y endpos)
                                                                       (pos-x endpos)))))
                                      (cond [(not (equal? (char->integer #\a) (pos-z c))) m]
                                            [(false? endpath) m]
                                            [else (min m (length endpath))])))
                          +inf.0 workgrid))
(println part-2)
