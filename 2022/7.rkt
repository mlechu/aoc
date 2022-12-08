#lang racket

(require "util.rkt")
(define input (fsplit 7 "$ " "\n" " "))

(struct dir (size children))
(struct leaf (size name))

(define (parse-command cmds cd)
  (let [(cmd (if (empty? cmds) "x" (car cmds)))]
    (match cmd
      ["x" (values cmds (dir (eval-size cd) (dir-children cd)))]
      [(list (list "cd" ".."))
       (values (cdr cmds) (dir (eval-size cd) (dir-children cd)))]
      [(cons (list "ls") contents)
       (parse-command (cdr cmds) (fill-dir cd contents))]
      [(list (list "cd" nd))
       (let-values (((c pf) (parse-folder (cdr cmds) (hash-ref (dir-children cd) nd))))
         (parse-command c (dir (dir-size cd)
                               (hash-set (dir-children cd) nd pf))))])))

(define (parse-folder cmds cd)
  (cond
    [(empty? cmds) (values cmds cd)]
    [else (parse-command cmds cd)]))

(define (lsln->h ln)
  (cons (second ln) (match (car ln)
                      ["dir" (dir 0 (hash))]
                      [(app string->number (? number? a))
                       (leaf (string->number (car ln)) (second ln))])))

(define (fill-dir cd contents)
  (dir (dir-size cd)
       (make-immutable-hash (map lsln->h contents))))

(define (eval-size f [acc 0])
  (+ acc (match f
           [(? leaf? f) (leaf-size f)]
           [(? dir? f) (foldl eval-size 0 (hash-values (dir-children f)))])))

;; sum (lte 100k (self, r(filter dir children)))
(define (dirsum f p?)
  (cond
    [(leaf? f) 0]
    [(p? f) (apply + (cons (dir-size f) (map (lambda (c) (dirsum c p?)) (hash-values (dir-children f)))))]
    [else   (apply +                    (map (lambda (c) (dirsum c p?)) (hash-values (dir-children f))))]))

(define init (dir 0 (hash "/" (dir 0 (hash)))))
(define-values (_ fs) (parse-command input init))
(define part-1 (dirsum fs (lambda (d) (<= (dir-size d) 100000))))
part-1

(define disk-space 70000000)
(define need-unused 30000000)
(define to-delete (- (dir-size fs) (- disk-space need-unused))) ;; 4965705

(define (min-gte l) (apply min (filter (lambda (x) (>= x to-delete)) l)))

(define (search-gte d)
  (println to-delete)
  ;; (if (dir? d) (println (>= (dir-size d) to-delete)) (println 0))
  ;; (if (dir? d) (println (dir-size d)) (println 0))
  ;; (if (dir? d) (println (cons (dir-size d) (map search-gte (hash-values (dir-children d))))) (print 0 ))
  ;; (if (dir? d) (println (min-gte (cons (dir-size d) (map search-gte (hash-values (dir-children d)))))) (print ""))
  ;; i calculated to-delete wrong haha.................
  (cond
    [(leaf? d) +inf.0]
    [else (min-gte (cons (dir-size d) (map search-gte (hash-values (dir-children d)))))]))

(define part-2 (search-gte fs))
part-2
