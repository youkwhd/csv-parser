#lang typed/racket

(require "io.rkt")

(: csv-split (-> String Char (Listof String)))
(define (csv-split str sep)
  (let split-all ([chars : (Listof Char) (string->list str)])
    (: result (Pairof String Integer))
    (define result
      (let split-until-sep ([chars : (Listof Char) chars]
                            [buffer : String ""]
                            [iteration : Integer 0]
                            [ignore-sep? : Boolean #f])
        (cond
          [(empty? chars) (cons buffer iteration)]
          [(and (not ignore-sep?) (equal? (first chars) sep)) (cons buffer (+ iteration 1))]
          [(equal? (first chars) #\newline) (split-until-sep  (rest chars) buffer (+ iteration 1) ignore-sep?)]
          [(equal? (first chars) #\") (split-until-sep (rest chars) buffer (+ iteration 1) (not ignore-sep?))]
          [else
            (split-until-sep (rest chars) (string-append buffer (string (first chars))) (+ iteration 1) ignore-sep?)])))
    (cond
      [(empty? chars) '()]
      [else
        (cons (car result) (split-all (list-tail chars (cdr result))))])))

(: csv-header-split? (-> String Boolean))
(define (csv-header-split? header)
  (and 
    (equal? (string-ref header 0) #\")
    (not (equal? (string-ref header (- (string-length header) 1)) #\"))))

;; (: parse-csv-header (-> (Listof String) (Pair Integer (Listof String))))
(: parse-csv-header (-> (Listof String) (Listof String)))
(define (parse-csv-header lines)
  ;; (: parsed-header (Listof String))
  ;; (define parsed-header 
    (let combine-header ([headers : (Listof String) (string-split (first lines) "," #:trim? #f)]
                         [traversed : Integer 1]
                         [parsed? : Boolean #f])
      (cond
        [parsed? '()]
        [(equal? (string-ref (first headers) 1) #\") '()]
        [else
          (cons (first headers) (combine-header (rest headers) 1 #f))])))

(: read-csv (-> String (Listof (Listof String))))
(define (read-csv path)
  (let ([lines : (Listof String) (string-split path "\n")])
    (displayln (parse-csv-header lines))
    '()))

;; (read-csv (read-file "currency.csv"))
