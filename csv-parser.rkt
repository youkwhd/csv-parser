#lang typed/racket

(require "io.rkt")

(: boolean->integer (-> Boolean Integer))
(define (boolean->integer bool)
  (if bool 1 0))

(: parse-line (-> String (Listof String)))
(define (parse-line csv)
  '())

(: string-empty? (-> String Boolean))
(define (string-empty? str)
  (equal? (string-length str) 0))

(: string-find (-> String String Integer))
(define (string-find str target)
  (let loop ([str : String str]
             [index : Integer 0])
    (cond
      [(string-empty? str) -1]
      [(string=? (substring str 0 (string-length target)) target) index]
      [else (loop (substring str 1) (+ index 1))])))

(: string-find-either (-> String String String Integer))
(define (string-find-either str t1 t2)
  (let loop ([str : String str]
             [index : Integer 0])
    (cond
      [(string-empty? str) -1]
      [(string=? (substring str 0 (string-length t1)) t1) index]
      [(string=? (substring str 0 (string-length t2)) t2) index]
      [else (loop (substring str 1) (+ index 1))])))

(: quote-wrapped? (-> String Boolean))
(define (quote-wrapped? str)
  #t)

(: parse-csv (-> String (Listof String)))
(define (parse-csv csv)
  (let loop ([csv : String csv])
    (define quote-wrapped? (string-prefix? csv "\""))
    (define index-of-delim (if quote-wrapped? (string-find csv "\",") (string-find-either csv "," "\n")))
    (cond
      [(equal? index-of-delim -1) '()]
      [else
        ;; (displayln (substring csv (+ 0 (boolean->integer quote-wrapped?)) index-of-delim))
        ;; (displayln "")
        (cons (substring csv (+ 0 (boolean->integer quote-wrapped?)) index-of-delim) (loop (substring csv (+ (+ index-of-delim 1) (boolean->integer quote-wrapped?)))))])))

;; (parse-csv (read-file "currency.csv"))
(parse-csv (read-file "samples/food.csv"))
;; (parse-csv (read-file "color.csv"))
