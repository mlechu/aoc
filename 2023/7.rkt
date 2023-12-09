#lang racket

(require "util.rkt")
(define input (fsplit 7 "\n" " "))

(define (hand->ht h)
  (foldl (λ (c ht) (hash-update ht c add1 0)) (hash) h))

(define (card->rank c)
  (match c
    [#\2 0]
    [#\3 1]
    [#\4 2]
    [#\5 3]
    [#\6 4]
    [#\7 5]
    [#\8 6]
    [#\9 7]
    [#\T 8]
    [#\J 9]
    [#\Q 10]
    [#\K 11]
    [#\A 12]))

(define (chrcmp c1 c2)
  (< (card->rank c1) (card->rank c2)))

(define (strcmp proc s1 s2)
  (cond [(empty? s2) #f]
        [(proc (first s1) (first s2)) #t]
        [(proc (first s2) (first s1)) #f]
        [(equal? (first s1) (first s2)) (strcmp proc (cdr s1) (cdr s2))]))

(define ranked
  (sort
   input
   (λ (i j)
     (match-let (((list (app string->list hand1) _) i)
                 ((list (app string->list hand2) _) j))
       (define ht1 (hand->ht hand1))
       (define ht2 (hand->ht hand2))
       (define diff1 (set-subtract (hash-values ht1) (hash-values ht2)))
       (define diff2 (set-subtract (hash-values ht2) (hash-values ht1)))
       (cond
         [(> (hash-count ht1) (hash-count ht2)) #t]
         [(< (hash-count ht1) (hash-count ht2)) #f]
         [(set-member? diff2 4) #t]
         [(set-member? diff1 4) #f]
         [(set-member? diff2 3) #t]
         [(set-member? diff1 3) #f]
         [else (strcmp chrcmp hand1 hand2)])))))

(define ranked-c (map cons (range 1 (add1 (length ranked))) ranked))
;; ranked-c
(define p1 (foldl (λ (h s) (+ s (* (first h) (string->number (third h))))) 0 ranked-c))
p1

(define (card->rank-2 c)
  (match c
    [#\J -1]
    [#\2 0]
    [#\3 1]
    [#\4 2]
    [#\5 3]
    [#\6 4]
    [#\7 5]
    [#\8 6]
    [#\9 7]
    [#\T 8]
    [#\Q 10]
    [#\K 11]
    [#\A 12]))

(define (chrcmp-2 c1 c2)
  (< (card->rank-2 c1) (card->rank-2 c2)))

(define (hand->ht-jokerfied h)
  (let ((ht (hand->ht h)))
    (cond [(and (hash-has-key? ht #\J) (< 1 (hash-count ht)))
           (make-immutable-hash
            (match (sort (hash->list (hash-remove ht #\J))
                         (λ (a b) (> (cdr a) (cdr b))))
              [`((,most . ,cnt) ,@x*) `((,most . ,(+ cnt (hash-ref ht #\J))) ,@x*)]))]
          [else ht])))

(define ranked-2
  (sort
   input
   (λ (i j)
     (match-let (((list (app string->list hand1) _) i)
                 ((list (app string->list hand2) _) j))
       (define ht1 (hand->ht-jokerfied hand1))
       (define ht2 (hand->ht-jokerfied hand2))
       (define diff1 (set-subtract (hash-values ht1) (hash-values ht2)))
       (define diff2 (set-subtract (hash-values ht2) (hash-values ht1)))
       (cond
         [(> (hash-count ht1) (hash-count ht2)) #t]
         [(< (hash-count ht1) (hash-count ht2)) #f]
         [(set-member? diff2 4) #t]
         [(set-member? diff1 4) #f]
         [(set-member? diff2 3) #t]
         [(set-member? diff1 3) #f]
         [else (strcmp chrcmp-2 hand1 hand2)])))))


(define ranked-c-2 (map cons (range 1 (add1 (length ranked-2))) ranked-2))
ranked-c-2
(define p2 (foldl (λ (h s) (+ s (* (first h) (string->number (third h))))) 0 ranked-c-2))
p2
