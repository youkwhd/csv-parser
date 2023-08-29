#lang typed/racket

(: boolean->integer (-> Boolean Integer))
(define (boolean->integer bool)
  (if bool 1 0))

(provide (all-defined-out))
