#lang typed/racket

(: string-empty? (-> String Boolean))
(define (string-empty? str)
  (equal? (string-length str) 0))

(: quote-capsulated? (-> String Boolean))
(define (quote-capsulated? str)
    (and (>= (string-length str) 2) (string-prefix? str "\"") (string-suffix? str "\"")))

(: remove-quote-capsulated (-> String String))
(define (remove-quote-capsulated str)
  (if (quote-capsulated? str)
    (substring str 1 (- (string-length str) 1))
    str))

(: trim-quote-capsulated (-> String String))
(define (trim-quote-capsulated str)
  (let ([trimmed-str : String (string-trim str)])
    (if (quote-capsulated? trimmed-str)
      trimmed-str
      str)))

(: transform-double-quote (-> String String))
(define (transform-double-quote str)
  (let loop ([str : String str])
    (cond
      [(string-empty? str) ""]
      [(and (>= (string-length str) 2) (string=? (substring str 0 2) "\"\""))
       (string-append "\\\"" (loop (substring str 2)))]
      [else
        (string-append (substring str 0 1) (loop (substring str 1)))])))

(provide (all-defined-out))
