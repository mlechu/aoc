#lang racket

(define (fsplit fnum . splits)
  (rsplit ;;(list
           (file->string
            (string-join (list "./input/" (number->string fnum) ".txt") ""));;)
          splits))

;; each split is either a string delimiter or a procedure taking a string and returning a list of strings
;; final split arg can return anything
;; list, list -> ?

(define (rsplit l splits)
  (cond [(empty? splits) l]
        ;; [(procedure? (first splits)) (map (lambda (chunk) (rsplit chunk (cdr splits)))
        ;;                                   (apply (car splits) (list l)))]
        [(string? (first splits)) (map (lambda (chunk) (rsplit chunk (cdr splits)))
                                       (string-split l (car splits)))]))

;; (rsplit "iaiaiaibiaiaiaibiaiaiaiciaiaiaibiaiaiaibiaiaiaiciaiaiaiaiibiaiaiaic" (list "c" "b" "a"))
;; (fsplit 2 "\n" " ")

(define (chunk-list l n)
  (cond [(zero? n) (/ 1 0)]
        [(empty? l) '()]
        [(< (length l) n) (list l)]
        [else (cons (take l n) (chunk-list (list-tail l n) n))]))

(provide (all-defined-out))

;; (fsplit 4 "\n" "," "-" )
