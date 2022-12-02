#lang racket

(define elf-sums
  (sort
   (map (lambda (elf)
          (foldl (lambda (entry total) (+ total (string->number entry)))
                 0
                 (string-split elf "\n")))
        (string-split (file->string "./input/1.txt") "\n\n"))
   >))

;; (define part-1 (apply max elf-sums))
(define part-1 (car elf-sums))

part-1

(define part-2 (+ (first elf-sums) (second elf-sums) (third elf-sums)))

part-2
