#lang racket

(require "util.rkt")
(define input (map (λ (line) (drop (drop-right line 1) 1)) (fsplit 3 "\n" "")))

(define grid (lists->grid input))

(define (neighbours pos)
  (match pos
    [`(,y ,x)
     (apply append
            (map (λ (ya)
                   (map (λ (xa) `(,(+ y ya)
                                  ,(+ x xa)))
                        (range -1 2)))
                 (range -1 2)))]))

(define (good-spots grid)
  (define ht (make-hash))
  (for ([y (range (grid-height grid))])
    (for ([x (range (grid-width grid))])
      (match (grid-ref grid y x)
        [(app string->number (? number? n)) #f]
        ["." #f]
        [ch (apply hash-set*! ht
                   (append-map (λ (n) `(,n ,ch))
                               (neighbours `(,y ,x))))])))
  ht)

(define (get-num line x)
  (match (memf (λ (c) (number? (string->number c))) line)
    [#f (values #f '() line)]
    [l (let* ((end (match (memf (λ (c) (not (number? (string->number c)))) l)
                     [#f '()]
                     [e e]))
              (skippedlen (- (grid-width grid) (length l)))
              (nlen (- (length l) (length end)))
              (n (string->number (string-join (take l nlen) "")))
              (poslist (range skippedlen (+ skippedlen nlen))))
         (values n poslist end))]))

(define (line->nums line y x)
  (let-values ([(n poslist next) (get-num line x)])
    (match n
      [#f '()]
      [n (cons `(,n ,(map (λ (p) (list y p)) poslist))
               (line->nums next y (+ x (length poslist))))])))

(define (number-table input)
  ;; (123 ((x y) (x y) (x y)))
  (apply append (for/list ([row input] [y (range (length input))])
                  (line->nums row y 0))))

(define (p1 input grid)
  (define gst (good-spots grid))
  ;; (print gst)
  ;; (print (number-table))
  (apply
   + (map
      (λ (n-pos*)
        (let ((ok (ormap identity
                         (map (λ (pos) (hash-has-key? gst pos))
                              (second n-pos*)))))
          (cond
            [ok (first n-pos*)]
            [#t 0]))) (number-table input))))

(p1 input grid)

(define (p2 input grid)

  (define (gear-hl grid)
    (define ht (make-hash))
    (for ([y (grid-height grid)])
      (for ([x (grid-width grid)])
        (if (equal? "*" (grid-ref grid y x))
            (hash-set! ht `(,y ,x) "*")
            (void))))
    (hash->list ht))

  (define nt (number-table input))

  (define (n-neighbours pos grid)
    (let ((ns (neighbours pos)))
      (filter (λ (n-pos*) (not (empty? (set-intersect ns (second n-pos*))))) nt)))

  #;(displayln (filter (λ (gl) (= 2 (length gl)))
          (map (λ (g) (n-neighbours (car g) grid))
               (gear-hl grid))))


  
  (apply + (map (λ (gp) (* (first (first gp)) (first (second gp))))
                (filter (λ (gl) (= 2 (length gl)))
                        (map (λ (g) (n-neighbours (car g) grid))
                             (gear-hl grid))))))

(p2 input grid)
