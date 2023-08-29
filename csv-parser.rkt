#lang typed/racket

(require "io.rkt")
(require "string.rkt")

(: parse-field (-> String (Pairof (Pairof String String) Boolean)))
(define (parse-field str)
  (: field-metadata (Pairof Integer Boolean))
  (define field-metadata
    (let field-metadata ([nth : Integer 0]
                         [inside-quote? : Boolean #f]
                         [str : String str])
      (cond
        [(string-empty? str) (cons nth #t)]
        [(and (string-prefix? str "\n") (not inside-quote?)) (cons nth #t)]
        [(and (string-prefix? str ",") (not inside-quote?)) (cons nth #f)]
        [(string-prefix? str "\\\"") (field-metadata (+ nth 2) inside-quote? (substring str 2))]
        [(string-prefix? str "\"") (field-metadata (+ nth 1) (not inside-quote?) (substring str 1))]
        [else (field-metadata (+ nth 1) inside-quote? (substring str 1))])))

  (cons
    (cons (substring str 0 (car field-metadata)) 
          (substring str (if (> (+ (car field-metadata) 1) (string-length str))
                           (string-length str)
                           (+ (car field-metadata) 1))))
    (cdr field-metadata)))

(: parse-csv-raw (-> String (Listof (Listof String))))
(define (parse-csv-raw str)
  (cond
    [(string-empty? str) '()]
    [else
      (: parsed-fields (Pairof (Listof String) String))
      (define parsed-fields 
        (let parse-line ([str : String str]
                         [fields : (Listof String) '()])
          (: field (Pairof (Pairof String String) Boolean))
          (define field (parse-field str))

          (cond
            [(cdr field)
             (cons (append fields (list (caar field))) (cdar field))]
            [else
              (parse-line (cdar field) (append fields (list (caar field))))])))
      (cons (car parsed-fields) (parse-csv-raw (cdr parsed-fields)))]))

(: parse-csv (-> String (Listof (Listof String))))
(define (parse-csv str)
  (map (lambda ([fields : (Listof String)]) (map remove-quote-capsulated fields)) (parse-csv-raw str)))

(parse-csv (read-file "samples/food.csv"))
;; (parse-csv (read-file "samples/currency.csv"))
;; (parse-csv (read-file "samples/color.csv"))
