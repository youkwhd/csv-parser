#lang typed/racket

(require "io.rkt")

(: boolean->integer (-> Boolean Integer))
(define (boolean->integer bool)
  (if bool 1 0))

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

(: parse-csv (-> String (Listof (Listof String))))
(define (parse-csv csv-t)
  (cond
    [(string-empty? csv-t) '()]
    [else
      (: a (Listof String))
      (define a 
        (let loop ([csv : String csv-t])
          (define quote-wrapped? (string-prefix? csv "\""))
          (define index-of-delim (if quote-wrapped? (string-find-either csv "\"," "\"\n") (string-find-either csv "," "\n")))
          (define end-of-row? (or 
                                (and (= (string-find csv ",") -1) (not (= (string-find csv "\n") -1)))
                                (and (not (= (string-find csv ",") -1)) (< (string-find csv "\n") (string-find csv ",")))))
          (cond
            [(equal? index-of-delim -1) 
             (set! csv-t "")
             '()]
            [end-of-row? 
              (set! csv-t (substring csv (+ (+ index-of-delim 1) (boolean->integer quote-wrapped?))))
              (cons (substring csv (+ 0 (boolean->integer quote-wrapped?)) index-of-delim) '())]
            [else
              (cons (substring csv (+ 0 (boolean->integer quote-wrapped?)) index-of-delim) (loop (substring csv (+ (+ index-of-delim 1) (boolean->integer quote-wrapped?)))))])))
      (cons a (parse-csv csv-t))]))

;; (parse-csv (read-file "currency.csv"))
;; (parse-csv (read-file "samples/food.csv"))

(parse-csv (read-file "color.csv"))
