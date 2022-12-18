#lang racket

(require "util.rkt")
(define input (fsplit 11 "\n\n" "\n" string-trim))

(struct monkey (id items op ntest inspections))
(define (para->monkey p)
  (monkey
   (string->number (string-trim (string-trim (first p) ":") "Monkey "))
   (map string->number
        (string-split (string-trim (second p)
                                   "Starting items: ")
                      ", "))
   (eval `(lambda (old)
            ,(let ((op (string-split (string-trim (third p)
                                                  "Operation: new = ") " "))
                   (operand (λ (o) (if (equal? "old" o)
                                       `old
                                       (string->number o)))))
               (list (string->symbol (second op))
                     (operand (first op))
                     (operand (third op)))))
         (make-base-namespace))
   (λ (w) (cond
            [(zero?
              (modulo w
                      (string->number
                       (string-trim (fourth p)
                                    "Test: divisible by "))))
             (string->number (string-trim (fifth p)
                                          "If true: throw to monkey "))]
            [else (string->number
                   (string-trim (sixth p)
                                "If false: throw to monkey "))]))
   0))

(define mlist-initial (vector-map para->monkey (list->vector input)))

(define (/3 n) (floor (/ n 3)))

(define (turn id mlist)
  (let ((m (vector-ref mlist id)))
    (cond
      [(empty? (monkey-items m)) mlist]
      [else (let* ((item (/3 (apply (monkey-op m)
                                    (list (car (monkey-items m))))))
                   (nextid (apply (monkey-ntest m) (list item)))
                   (nextm (vector-ref mlist nextid))
                   (nextm+ (monkey nextid
                                   (append (monkey-items nextm) (list item))
                                   (monkey-op nextm)
                                   (monkey-ntest nextm)
                                   (monkey-inspections nextm))))
              (turn id
                    (vector-set (vector-set mlist nextid nextm+)
                                id
                                (monkey id
                                        (cdr (monkey-items m))
                                        (monkey-op m)
                                        (monkey-ntest m)
                                        (add1 (monkey-inspections m))))))])))

(define turns
  (* 20 (vector-length mlist-initial)))

(define tlist
  (build-list turns (λ (t) (modulo t (vector-length mlist-initial)))))

(define p1m
  (sort (map monkey-inspections (vector->list (foldl turn mlist-initial tlist))) >))

(define part-1
  (* (first p1m) (second p1m)))

part-1

;; 2: an item is now a list of numbers mod monkey test
(define modlist
  (map (λ (i)
         (string->number
          (string-trim
           (fourth i)
           "Test: divisible by "))) input))

(define (n->mclasses n)
  (map (λ (mc) (modulo n mc)) modlist))

(define (normalize-m ns)
  (map (λ (n m) (modulo n m)) ns modlist))

(define mlist-initial2
  (vector-map (λ (m)
                (monkey (monkey-id m)
                        (map n->mclasses (monkey-items m))
                        (monkey-op m)
                        (monkey-ntest m)
                        (monkey-inspections m)))
              mlist-initial))

(define (turn2 id mlist)
  (let ((m (vector-ref mlist id)))
    (cond
      [(empty? (monkey-items m)) mlist]
      [else (let* ((item (normalize-m (map (monkey-op m) (car (monkey-items m)))))
                   (nextid (apply (monkey-ntest m) (list (list-ref item id))))
                   (nextm (vector-ref mlist nextid))
                   (nextm+ (monkey nextid
                                   (append (monkey-items nextm) (list item))
                                   (monkey-op nextm)
                                   (monkey-ntest nextm)
                                   (monkey-inspections nextm))))
              (turn2 id
                     (vector-set (vector-set mlist nextid nextm+)
                                 id
                                 (monkey id
                                         (cdr (monkey-items m))
                                         (monkey-op m)
                                         (monkey-ntest m)
                                         (add1 (monkey-inspections m))))))])))

(define turns2 (* 10000 (vector-length mlist-initial2)))
(define tlist2 (build-list turns2
                           (λ (t)  (modulo t (vector-length mlist-initial2)))))
(define p2m (sort (map monkey-inspections (vector->list (foldl turn2 mlist-initial2 tlist2))) >))
(define part-2 (* (first p2m) (second p2m)))
part-2
