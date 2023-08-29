#lang typed/racket

(: string-empty? (-> String Boolean))
(define (string-empty? str)
  (equal? (string-length str) 0))

(provide (all-defined-out))
