#lang typed/racket

(define-type Point (List Integer Integer))
(define-type SB (List Point Point))

;; (require/typed "util.rkt"
;;                [fsplit (-> Integer (U String Procedure) * (Listof (List Point Point)))])

;; https://github.com/racket/typed-racket/issues/607

(: input (Listof SB))
(define input ; not good
  (cast
   (map (λ ([s : String])
          (map (λ ([s2 : String])
                 (map (λ ([s3 : String])
                        (string->number (string-trim (string-trim s3 "y=") "Sensor at x=")))
                      (string-split s2 ", ")))
               (string-split s ": closest beacon is at x=")))
        (string-split (file->string "./input/15.txt") "\n"))
   (Listof SB)))

(struct interval ([start : Integer] [end : Integer]))

(: interval-joinable? (-> interval interval Boolean))
(define (interval-joinable? a b)
  (or (and (<= (interval-start b) (add1 (interval-end a)))
           (>= (interval-end b) (interval-end a)))
      (and (<= (interval-start a) (add1 (interval-end b)))
           (>= (interval-end a) (interval-end b)))))

(: join-intervals (-> interval (Listof interval) (Listof interval)))
(define (join-intervals i is)
  (match/values
   (partition (curry interval-joinable? i) is)
   [('() '()) (list i)]
   [('() n) (cons i n)]
   [(y n) (cons (interval (apply min (map interval-start (cons i y)))
                          (apply max (map interval-end (cons i y))))
                n)]))

(: md (-> Point Point Integer))
(define (md s b) (+ (abs (- (first s) (first b)))
                    (abs (- (second s) (second b)))))

(: project-interval (-> Integer (List Point Point) (U #f interval)))
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

(: beacons (HashTable Point Integer))
(define beacons
  (foldl (λ ([b : Point]
             [ht : (HashTable Point Integer)])
           (hash-update ht b (λ ([i : Integer]) (add1 i)) (λ _ 0)))
         (ann (hash) (HashTable Point Integer))
         (map (λ ([x : SB]) (second x)) input)))

(define 1y 2000000)

(define part-1
  (- (foldl + 0
            (map (λ ([i : interval]) (- (interval-end i) (interval-start i)))
                 (foldl join-intervals '()
                        (filter-map (curry project-interval 1y) input))))
     (count (λ ([j : Integer]) (= j 1y))
            (map (λ ([b : Point]) (second b)) (hash-keys beacons)))))

part-1

;; if there is only one, it must be bounded on four sides
;; by rhombi or the search space walls
(define smax 4000000)

(: edge? (-> Integer Boolean))
(define (edge? coord) (or (zero? coord) (= smax coord)))

(: outer-ring (-> SB (HashTable Point Integer) (HashTable Point Integer)))
(define (outer-ring sb h)
  (displayln "\tadding a sensor...")
  (foldl (λ ([k : Point]
             [hi : (HashTable Point Integer)])
           (cond
             [(or (< (first k) 0) (> (first k) smax)) hi]
             [(or (< (second k) 0) (> (second k) smax)) hi]
             [else (hash-update hi k add1 (λ _
                                            (cond [(and (edge? (first k)) (edge? (second k))) 3]
                                                  [(or (edge? (first k)) (edge? (second k))) 3]
                                                  [else 0])))]))
         h
         (let* ((d (apply md sb))
                (od (add1 d))
                (sx (first (first sb)))
                (sy (second (first sb))))
           (append (build-list od (λ ([i : Integer]) (list (+ (- sx od) i) (- sy i))))     ; l / #
                   (build-list od (λ ([i : Integer]) (list (- (+ sx od) i) (+ sy i))))     ; r # /
                   (build-list od (λ ([i : Integer]) (list (+ sx i) (+ (- sy od) i))))     ; t # \
                   (build-list od (λ ([i : Integer]) (list (- sx i) (- (+ sy od) i)))))))) ; b \ #

(: candidates (Listof Point))
(define candidates
  (map (λ ([kv : (Pairof Point Integer)]) (car kv))
       (filter (λ ([kv : (Pairof Point Integer)]) (<= 4 (cdr kv)))
               (hash->list (foldl outer-ring
                                  (ann (hash) (HashTable Point Integer))
                                  input)))))

;; (define candidates
;;   '((2526641 4000000) (4000000 3725363) (2655411 3166538) (3872908 3598271) (3872907 3598272)))

(: tuning-freq (-> Point Integer))
(define (tuning-freq p) (+ (* 4000000 (first p)) (second p)))

(define part-2
  (tuning-freq
   (first
    (filter (λ ([c : Point])
              (andmap (λ ([i : SB])
                        (> (md c (first i))
                           (apply md i))) input))
            candidates))))

part-2
