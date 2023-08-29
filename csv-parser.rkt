#lang typed/racket

(require "io.rkt")
(require "string.rkt")

(: f (-> String (Listof (Listof String))))
(define (f str)
  (cond
    [(string-empty? str) '()]
    [else
      (: zz (Pairof (Listof String) String))
      (define zz (let parse-line ([str : String str]
                                  [fields : (Listof String) '()])
        (: field-length (Pairof Integer Boolean))
        (define field-length (let find-length ([nth : Integer 0]
                                               [inside-quote? : Boolean #f]
                                               [str : String str])
                               (cond
                                 [(string-empty? str) (cons nth #t)]
                                 [(and (string-prefix? str "\n") (not inside-quote?)) (cons nth #t)]
                                 [(and (string-prefix? str ",") (not inside-quote?)) (cons nth #f)]
                                 [(string-prefix? str "\\\"") (find-length (+ nth 2) inside-quote? (substring str 2))]
                                 [(string-prefix? str "\"") (find-length (+ nth 1) (not inside-quote?) (substring str 1))]
                                 [else (find-length (+ nth 1) inside-quote? (substring str 1))])))
        (: b (Pairof String String))
        (define b
          (let parse-field ([str : String str]
                            [field : String ""])
            (cond
              [(string-empty? str) (cons field "")]
              [else
                (cons (substring str 0 (car field-length)) (substring str (if (> (+ (car field-length) 1) (string-length str))
                                              (string-length str)
                                              (+ (car field-length) 1))))])))

        (cond
          [(cdr field-length)
           (cons (append fields (list (car b))) (cdr b))]
          [else
            (parse-line (cdr b) (append fields (list (car b))))])))
      (cons (car zz) (f (cdr zz)))]))

(f (read-file "currency.csv"))

;; (f (read-file "currency.csv"))
;; (f (read-file "samples/food.csv"))
;; (f (read-file "color.csv"))
