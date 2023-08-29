#lang typed/racket

(: string-empty? (-> String Boolean))
(define (string-empty? str)
  (equal? (string-length str) 0))

(: remove-quote-capsulated (-> String String))
(define (remove-quote-capsulated str)
  (if (and (>= (string-length str) 2) (string-prefix? str "\"") (string-suffix? str "\""))
    (substring str 1 (- (string-length str) 1))
    str))

(provide (all-defined-out))
