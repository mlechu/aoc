#lang racket

(require "util.rkt")
(define input (fsplit 15 "\n" ": closest beacon is at x=" ", "
                      (curryr string-trim "Sensor at x=")
                      (curryr string-trim "y=")
                      string->number))

(struct interval (start end))

(define (interval-joinable? a b)
  (or (and (<= (interval-start b) (add1 (interval-end a)))
           (>= (interval-end b) (interval-end a)))
      (and (<= (interval-start a) (add1 (interval-end b)))
           (>= (interval-end a) (interval-end b)))))

(define (join-intervals i is)
  (match/values
   (partition (curry interval-joinable? i) is)
   [('() '()) (list i)]
   [('() n) (cons i n)]
   [(y n) (cons (interval (apply min (map interval-start (cons i y)))
                          (apply max (map interval-end (cons i y))))
                n)]))

(define (md s b) (+ (abs (- (first s) (first b)))
                    (abs (- (second s) (second b)))))

(define (project-interval y sb)
  (let* ((sensor (first sb))
         (beacon (second sb))
         (sx (first sensor))
         (dy (abs (- (second sensor) y)))
         (i0 (interval (- sx (md sensor beacon))
                       (add1 (+ sx (md sensor beacon)))))
         (id (interval (+ (interval-start i0) dy)
                       (- (interval-end i0) dy))))
    (cond [(< (interval-start id) (interval-end id)) id]
          [else #f])))

(define beacons
  (foldl (λ (b ht) (hash-update ht b add1 0))
         (hash)
         (map second input)))

(define 1y 2000000)

(define part-1
  (- (foldl + 0
            (map (λ (i) (- (interval-end i) (interval-start i)))
                 (foldl join-intervals '()
                        (filter-map (curry project-interval 1y) input))))
     (count (curry = 1y)
            (map second (hash-keys beacons)))))

part-1

;; if there is only one, it must be bounded on four sides
;; by rhombi or the search space walls
(define smax 4000000)

(define (outer-ring sb h)
  (displayln "\tadding a sensor...")
  (foldl (λ (k hi)
           (cond
             [(or (< (first k) 0) (> (first k) smax)) hi]
             [(or (< (second k) 0) (> (second k) smax)) hi]
             [else (hash-update hi k add1 (match k
                                            [(list (or (== smax) 0) (or (== smax) 0)) 3]
                                            [(list-no-order (or (== smax) 0) _) 2]
                                            [else 0]))]))
         h
         (let* ((d (apply md sb))
                (od (add1 d))
                (sx (first (first sb)))
                (sy (second (first sb))))
           (append (build-list od (λ (i) (list (+ (- sx od) i) (- sy i))))     ; l / #
                   (build-list od (λ (i) (list (- (+ sx od) i) (+ sy i))))     ; r # /
                   (build-list od (λ (i) (list (+ sx i) (+ (- sy od) i))))     ; t # \
                   (build-list od (λ (i) (list (- sx i) (- (+ sy od) i)))))))) ; b \ #

(define candidates
  (map car
       (filter (λ (kv) (<= 4 (cdr kv)))
               (hash->list (foldl outer-ring (hash) input)))))

;; (define candidates
;;   '((2526641 4000000) (4000000 3725363) (2655411 3166538) (3872908 3598271) (3872907 3598272)))

(define (tuning-freq p) (+ (* 4000000 (first p)) (second p)))

(define part-2
  (tuning-freq
   (first
    (filter (λ (c) (andmap (λ (i) (> (md c (first i))
                                     (apply md i))) input))
            candidates))))

part-2
