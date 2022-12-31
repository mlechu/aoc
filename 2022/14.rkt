#lang racket

(require "util.rkt")
(define input (fsplit 14 "\n" " -> " "," string->number))
(define min-x (apply min (map first (flatten1 input))))
(define min-y (apply min (map second (flatten1 input))))
(define max-x (apply max (map first (flatten1 input))))
(define max-y (add1 (apply max (map second (flatten1 input)))))

(define air 0)
(define rock 1)
(define restable 2)
(define visitable 4)

;; sand will sit still at (y, x) if it is all three:
;;  - not rock
;;  - restable: each of (y+1, x-1), (y+1, x) (y+1, x+1) is either:
;;      - rock
;;      - restable
;;  - visitable: (y, x) is not rock, any one of
;;      - (y, x) is the sand spawner
;;      - (y-1, x) is visitable
;;      - (y-1, x-1) is visitable and (y, x-1) is restable and (y, x-2) is restable
;;      - (y-1, x+1) is visitable and (y, x+1) is restable

(define (rockset-r pl [acc (hash)])
  (match pl
    [(list p) acc]
    [(cons (list x ay) (cons (list x by) r))
     (rockset-r (cdr pl)
                (foldl (λ (p hs) (hash-set hs p rock)) acc
                       (build-list (add1 (abs (- ay by)))
                                   (λ (i) (list (+ i (min ay by)) x)))))]
    [(cons (list ax y) (cons (list bx y) r))
     (rockset-r (cdr pl)
                (foldl (λ (p hs) (hash-set hs p rock)) acc
                       (build-list (add1 (abs (- ax bx)))
                                   (λ (i) (list y (+ i (min ax bx)))))))]))

(define rockset (foldl rockset-r (hash) input))

(define restset
  (foldl
   (λ (yx ht)
     (let ((_y (add1 (first yx)))
           (x  (second yx)))
       (match ht
         [(hash-table ((== yx) (== rock)) (_ _)...) ht]
         [(hash-table ((== (list _y (sub1 x))) (or (== rock) (== restable)))
                      ((== (list _y x)) (or (== rock) (== restable)))
                      ((== (list _y (add1 x))) (or (== rock) (== restable)))
                      (_ _)...)
          (hash-set ht yx restable)]
         [_ ht])))
   rockset
   (cartesian-product (reverse (build-list max-y identity))
                      (build-list max-x identity))))

(define flowset
  (foldl (λ (yx ht)
           ;; (println ht)
           (let* ((y (first yx))
                  (^y (sub1 y))
                  (x  (second yx)))
             (match* (ht restset)
               [(_ (hash-table ((== yx) (== rock)) (_ _)...))
                ht]
               [((hash-table ((== (list ^y x)) (== visitable)) (_ _)...) _)
                (hash-set ht yx visitable)]
               [((hash-table ((== (list ^y (sub1 x))) (== visitable)) (_ _)...)
                 (hash-table ((== (list y (sub1 x))) (or (== rock) (== restable)))
                             ((== (list y (- x 2))) (or (== rock) (== restable)))
                             (_ _)...))
                (hash-set ht yx visitable)]
               [((hash-table ((== (list ^y (add1 x))) (== visitable)) (_ _)...)
                 (hash-table ((== (list y (add1 x))) (or (== rock) (== restable))) (_ _)...))
                (hash-set ht yx visitable)]
               [(_ _) ht])))
         (hash '(0 500) visitable)
         (cartesian-product (build-list max-y identity)
                            (build-list max-x identity))))


;; (print-grid (build-grid (add1 max-y) (- max-x 420) (lambda (y x) (hash-ref rockset (list y (+ 420 x)) " "))) display)
;; (print-grid (build-grid (add1 max-y) (- max-x 420) (lambda (y x) (bitwise-ior (hash-ref restset (list y (+ 420 x)) 0)
;;                                                                               (hash-ref flowset (list y (+ 420 x)) 0)))) display)
;; (print-grid (build-grid (add1 max-y) (- max-x 420) (lambda (y x) (hash-ref flowset (list y (+ 420 x)) " "))) display)
(define part-1 (count (λ (k) (and (= (hash-ref restset k 0) restable)
                                  (= (hash-ref flowset k 0) visitable))) (hash-keys flowset)))
part-1

(require racket/hash)

(define ground (add1 max-y))

(define (fillset-r y lr acc)
  (cond
    [(= ground y) acc]
    [else (let*
              ((left (sub1 (first lr)))
               (right (add1 (last lr)))
               (r (filter (λ (rx) (and (false? (hash-ref rockset (list y rx) #f))
                                       (or (member (sub1 rx) lr)
                                           (member rx lr)
                                           (member (add1 rx) lr))))
                          (build-list (add1 (- right left)) (curry + left)))))
            (fillset-r (add1 y)
                       r
                       (hash-union (make-immutable-hash
                                    (map (λ (rx) (cons (list y rx) visitable))
                                         r))
                                   acc)))]))

(define fillset (fillset-r 1 '(500) (hash '(0 500) visitable)))
(define part-2 (count (λ (k) (= (hash-ref fillset k 0) visitable)) (hash-keys fillset)))
part-2

;; (print-grid (build-grid (+ 2 max-y) (- max-x 420) (lambda (y x) (hash-ref fillset (list y (+ 420 x)) " "))) display)
