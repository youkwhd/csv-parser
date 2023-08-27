#lang typed/racket

(: hello-world (-> Void))
(define (hello-world)
  (displayln "hello world"))

(hello-world)
