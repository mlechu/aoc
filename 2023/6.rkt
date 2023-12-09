#lang racket

(require "util.rkt")
(define input (map (curry filter identity)
                   (fsplit 6 "\n" " " string->number)))
input

(define (t-p->dist t p)
  (* p (- t p)))

(define (winning-dists t d)
  (filter (λ (dist) (> dist d))
          (map (curry t-p->dist t) (range t))))

(define p1
  (for/fold ([prod 1])
            ([time (first input)]
             [dist (second input)])
    (* prod (length (winning-dists time dist)))))

p1

(define input2 (map (compose
                     string->number
                     list->string
                     (curry filter char-numeric?))
                    (fsplit 6 "\n" string->list)))

(define p2
  (match-let ([(list t d) input2])
    (define bot (ceiling (- (/ t 2) (sqrt (- (/ (expt t 2) 4) d))) ))
    (define top (ceiling (+ (/ t 2) (sqrt (- (/ (expt t 2) 4) d))) )) ; [)
    (- top bot)))
input2
p2

;; (p)(t - p) > d
;; p^2 - tp + t^2/4 < - d + t^2/4
;; (p - t/2)^2 < t^2/4 - d
;; (p - t/2) < +- sqrt(t^2/4 - d)
;; p < + t/2 +- sqrt(t^2/4 - d)

;; min p > t/2 - sqrt(t^2/4 - d)
;; max p < t/2 + sqrt(t^2/4 - d)

;; (* 45925653 (- 57726992 45925653))
;; (* 11801340 (- 57726992 11801340))
;; 291117211762026
