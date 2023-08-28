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

(: string-chop (-> String String (Pairof String String)))
(define (string-chop str target)
  (let ([index-to-split : Integer (string-find str target)])
    (if (= index-to-split -1)
      (cons "" "")
      (cons (substring str 0 index-to-split) (substring str (+ index-to-split 1))))))

;; (: f (-> String (Listof (Listof String))))
(: f (-> String (Listof String)))
(define (f str)
  ;; (let )
  (cond
    [(empty? str) '()]
    [else
      (: field-length Integer)
      (define field-length (let find-length ([nth : Integer 0]
                                 [inside-quote? : Boolean #f]
                                 [str : String str])
                  (cond
                    [(string-empty? str) nth]
                    [(and (string-prefix? str "\n") (not inside-quote?)) (+ nth 1)]
                    [(and (string-prefix? str ",") (not inside-quote?)) (+ nth 1)]
                    [(string-prefix? str "\\\"") (find-length (+ nth 2) inside-quote? (substring str 2))]
                    [(string-prefix? str "\"") (find-length (+ nth 1) (not inside-quote?) (substring str 1))]
                    [else (find-length (+ nth 1) inside-quote? (substring str 1))])))
      (if (> field-length 0)
        (cons (substring str 0 (- field-length 1)) (f (substring str field-length)))
        '())]))

(f "\"ad,\n\ndr\",jakeaddr\n123,233")

(: parse-csv (-> String (Listof (Listof String))))
(define (parse-csv str)
  (cond
    [(string-empty? str)
     '()]
    [else
      (: parsed-line (Listof String))
      (define parsed-line
        (let parse-line ([line : String str])
          (define quote-wrapped? (string-prefix? line "\""))
          (define index-of-delim (if quote-wrapped? (string-find-either line "\"," "\"\n") (string-find-either line "," "\n")))
          (define end-of-row? (or 
                                (and (= (string-find line ",") -1) (not (= (string-find line "\n") -1)))
                                (and (not (= (string-find line ",") -1)) (< (string-find line "\n") (string-find line ",")))
                                (and quote-wrapped?)))
          (cond
            [(equal? index-of-delim -1) 
             (set! str line)
             '()]
            [end-of-row? 
              (set! str (substring line (+ (+ index-of-delim 1) (boolean->integer quote-wrapped?))))
              (cons (substring line (+ 0 (boolean->integer quote-wrapped?)) index-of-delim) '())]
            [else
              (cons (substring line (+ 0 (boolean->integer quote-wrapped?)) index-of-delim) (parse-line (substring line (+ (+ index-of-delim 1) (boolean->integer quote-wrapped?)))))])))
      (cons parsed-line (parse-csv str))]))

;; (parse-csv (read-file "currency.csv"))
;; (parse-csv (read-file "samples/food.csv"))
;; (parse-csv (read-file "color.csv"))
