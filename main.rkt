#lang racket

(require "interpreter.rkt")

; Test environment
(define env
  '((a 10)
    (b 20)
    (c 30)))


(displayln "TEST 1: Find variable a")
(displayln (value-of '(var-exp a) env))


(displayln "TEST 2: Find variable b")
(displayln (value-of '(var-exp b) env))


(displayln "TEST 3: Find variable c")
(displayln (value-of '(var-exp c) env))
