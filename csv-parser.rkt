#lang typed/racket

(require "string.rkt")

(: parse-field (-> String String (Pairof (Pairof String String) Boolean)))
(define (parse-field str delimiter)
  (: field-metadata (Pairof Integer Boolean))
  (define field-metadata
    (let field-metadata ([nth : Integer 0]
                         [inside-quote? : Boolean #f]
                         [str : String str])
      (cond
        [(string-empty? str) (cons nth #t)]
        [(and (string-prefix? str "\n") (not inside-quote?)) (cons nth #t)]
        [(and (string-prefix? str delimiter) (not inside-quote?)) (cons nth #f)]
        [(string-prefix? str "\\\"") (field-metadata (+ nth 2) inside-quote? (substring str 2))]
        [(string-prefix? str "\"") (field-metadata (+ nth 1) (not inside-quote?) (substring str 1))]
        [else (field-metadata (+ nth 1) inside-quote? (substring str 1))])))

  (cons
    (cons (substring str 0 (car field-metadata)) 
          (substring str (if (> (+ (car field-metadata) 1) (string-length str))
                           (string-length str)
                           (+ (car field-metadata) 1))))
    (cdr field-metadata)))

(: parse-csv (->* [String] [#:delimiter Char] (Listof (Listof String))))
(define (parse-csv str #:delimiter [delimiter : Char #\,])
  (cond
    [(string-empty? str) '()]
    [else
      (: parsed-fields (Pairof (Listof String) String))
      (define parsed-fields 
        (let parse-line ([str : String str]
                         [fields : (Listof String) '()])
          (: field (Pairof (Pairof String String) Boolean))
          (define field (parse-field str (string delimiter)))

          (cond
            [(cdr field)
             (cons (append fields (list (transform-double-quote (remove-quote-capsulated (caar field))))) (cdar field))]
            [else
              (parse-line (cdar field) (append fields (list (transform-double-quote (remove-quote-capsulated (caar field))))))])))
      (cons (car parsed-fields) (parse-csv (cdr parsed-fields) #:delimiter delimiter))]))

(provide (all-defined-out))
