#lang racket

(define (fsplit fnum . delims)
  (rsplit (file->string (string-join (list "./input/" (number->string fnum) ".txt") ""))
          delims))

(define (rsplit s delims)
  (cond [(empty? delims) s]
        [else (map (lambda (chunk) (rsplit chunk (cdr delims)))
                   (string-split s (car delims)))]))

;; (rsplit "iaiaiaibiaiaiaibiaiaiaiciaiaiaibiaiaiaibiaiaiaiciaiaiaiaiibiaiaiaic" (list "c" "b" "a"))
;; (fsplit 2 "\n" " ")

(define (chunk-list l n)
  (cond [(zero? n) (/ 1 0)]
        [(empty? l) '()]
        [(< (length l) n) (list l)]
        [else (cons (take l n) (chunk-list (list-tail l n) n))]))

(provide (all-defined-out))
