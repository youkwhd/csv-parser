#lang typed/racket

(: read-file (-> Path-String String))
(define (read-file path)
  (port->string (open-input-file path) #:close? #t))

(provide (all-defined-out))
