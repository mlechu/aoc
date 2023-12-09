#lang racket

(require "util.rkt")
(define input (fsplit 1 "\n"))

(define (line-val l0)
  (let* ((l (string->list l0))
         (n1 (findf char-numeric? l))
         (n2 (findf char-numeric? (reverse l))))
    (string->number
     (string-join
      (list
       (string n1)
       (string n2)) ""))))

(define p1
  (foldl + 0 (map line-val input)))
p1


(define digitmap '(("zero!!!" . "0")
                   ("one" . "1")
                   ("two" . "2")
                   ("three" . "3")
                   ("four" . "4")
                   ("five" . "5")
                   ("six" . "6")
                   ("seven" . "7")
                   ("eight" . "8")
                   ("nine" . "9")))

(define (string-find s sub)
  (if
   (string-contains? s sub)
   (string-length (first (string-split s sub #:trim? #f)))
   +inf.0))

(define (s-val s digitmap)
  (let* ((o (findf char-numeric? (string->list s)))
         (opos (string-find s (string o))))
    (let-values ([(bestd besti)
                  (for/fold ([acc_d "?"] [pos +inf.0])
                            ([dn digitmap])
                    (let ((test (string-find s (car dn))))
                      (values (if (< test pos) (car dn) acc_d) (min test pos))))])
      (if (< besti opos)
          (string->number (cdr (assoc bestd digitmap)))
          (string->number (string o)))
      #;(string-replace s bestd (cdr (assoc bestd digitmap)) #:all? #f))))

(define (string-reverse s) (list->string (reverse (string->list s))))

(define (line-val-2 l)
  (+ (* 10 (s-val l digitmap))
     (s-val (string-reverse l) (map (λ (dm) `(,(string-reverse (car dm)) . ,(cdr dm))) digitmap))))

(define p2
  (foldl + 0 (map line-val-2 input)))
p2

;; (define input-fixed )
