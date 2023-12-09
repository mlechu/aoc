;; Make racket file and fetch input for given day (default: latest)

#lang racket

(require net/url)
(require racket/exn)

(define day
  (match (current-command-line-arguments)
    [(vector (app string->number a)) #:when (number? a) a]
    [_ (date-day
        (seconds->date (- (current-seconds)
                          (* 3600 5)) #f))]))

(define input-file (format "input/~a.txt" day))
(define sol-file (format "~a.rkt" day))

(define challenge-input
  (port->string
   (get-pure-port
    (string->url
     (format "https://adventofcode.com/2023/day/~a/input" day))
    (list (format "Cookie: session=~a" (file->string "./session.txt"))))))

(define sol-template
  (format #<<STR
#lang racket

(require "util.rkt")
(define input (fsplit ~a "\n"))
input

STR
          day))

(define (guarded-write f s)
  (with-handlers ([exn? (λ (e) (displayln (exn-message e)))])
    (define p (open-output-file f))
    (display s p)
    (close-output-port p)))

(cond [(string-prefix? challenge-input
                       "Please don't repeatedly request")
       (displayln "Future input; not available")]
       [else (guarded-write input-file challenge-input)])

(guarded-write sol-file sol-template)
