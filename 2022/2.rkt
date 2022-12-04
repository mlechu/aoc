#lang racket
(require "./util.rkt")

;; (fsplit 2 "\n" " ")

(define (shape-score-1 s)
  (match s
    ["X" 1]
    ["Y" 2]
    ["Z" 3]))

;; rock     A X
;; paper    B Y
;; scissors C Z

(define (score-game-1 g a)
  (+ a
     (shape-score-1 (second g))
     (match g
       ['("A" "X") 3]
       ['("A" "Y") 6]
       ['("A" "Z") 0]

       ['("B" "X") 0]
       ['("B" "Y") 3]
       ['("B" "Z") 6]

       ['("C" "X") 6]
       ['("C" "Y") 0]
       ['("C" "Z") 3]
       )))

(foldl score-game-1 0 (fsplit 2 "\n" " "))

;; 7369 too low
;; (foldl (lambda (g sum) (+ sum (shape-score (second g)))) 0 (fsplit 2 "\n" " "))
;; (length (fsplit 2 "\n" " "))

(define (shape-score-2 g)
  (match g
       ['("A" "X") 3] ;; scissors
       ['("A" "Y") 1] ;; rock
       ['("A" "Z") 2] ;; paper

       ['("B" "X") 1] ;; rock
       ['("B" "Y") 2] ;; paper
       ['("B" "Z") 3] ;; scissors

       ['("C" "X") 2] ;; paper
       ['("C" "Y") 3] ;; scissors
       ['("C" "Z") 1] ;; rock
       ))

(define (score-game-2 g a)
  (+ a
     (shape-score-2 g)
     (match g
       [(list f "X") 0]
       [(list f "Y") 3]
       [(list f "Z") 6]
       )))

(foldl score-game-2 0 (fsplit 2 "\n" " "))
;; 18924 too high
