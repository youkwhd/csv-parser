#lang typed/racket

(require "io.rkt")

(struct csv-metadata ([body : (HashTable String (Listof String))]
                      [header : (Listof String)]
                      [n-header-line : Integer])
        #:transparent)

(: parse-csv-header (-> (Listof String) csv-metadata))
(define (parse-csv-header each-csv-line)
  (let ([ht : (HashTable String (Listof String)) (make-hash)])
    (csv-metadata 
      ht 
      (foldr
        (lambda ([field : String]
                 [fields : (Listof String)])
          (hash-set! ht field '())
          (cons field fields))
        '()
        (string-split (list-ref each-csv-line 0) "," #:trim? #f))
      1)))

(: parse-csv-body-line (-> String csv-metadata Void))
(define (parse-csv-body-line csv-body-line metadata)
  (let* ([values : (Listof String) (string-split csv-body-line ",")]
         [values-length : Integer (length values)])
    (let loop ([n : Integer 0]
               [values : (Listof String) values])
      (cond
        [(= n values-length) (void)]
        [else
          (hash-set! 
            (csv-metadata-body metadata) 
            (list-ref (csv-metadata-header metadata) n) 
            (append 
              (hash-ref (csv-metadata-body metadata) (list-ref (csv-metadata-header metadata) n))
              (list (first values))))
          (loop (+ n 1) (rest values))]))))

(: parse-csv-body (-> (Listof String) csv-metadata Void))
(define (parse-csv-body csv-body-lines metadata)
  (for-each 
    (lambda ([line : String]) (parse-csv-body-line line metadata))
    csv-body-lines)
  (void))

(: read-csv (-> Path-String (HashTable String (Listof String))))
(define (read-csv path)
  (let* ([each-line : (Listof String) (string-split (read-file path) "\n")]
         [metadata : csv-metadata (parse-csv-header each-line)])
    (parse-csv-body (list-tail each-line (csv-metadata-n-header-line metadata)) metadata)
    (csv-metadata-body metadata)))

(read-csv "samples/food.csv")
