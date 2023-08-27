#lang typed/racket

(require "io.rkt")

(: parse-csv-header (-> String (HashTable String (Listof String))))
(define (parse-csv-header csv)
  (let ([ht : (HashTable String (Listof String)) (make-hash)]
        [each-line : (Listof String) (string-split csv "\n" #:trim? #f)])
    (for-each 
      (lambda ([field : String])
        (hash-set! ht field '()))
      (string-split (list-ref each-line 0) "," #:trim? #f))
    ht))

(: read-csv (-> Path-String (HashTable String (Listof String))))
(define (read-csv path)
  (parse-csv-header (read-file path)))

(displayln (read-csv "samples/food.csv"))
