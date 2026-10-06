#lang racket

(require "parser.rkt")
(require "interpreter.rkt")
(require "util.rkt")


(displayln "==============================")
(displayln "FUNCTION TEST")
(displayln "==============================")

(newline)


; ==========================================
; FUNCTION
; ==========================================

(define test-function
  '((function
      (a b)
      ((a * b)))
    (4 5)))


; ==========================================
; SHOW ORIGINAL
; ==========================================

(displayln "Original function:")

(displayln test-function)

(newline)


; ==========================================
; PARSE
; ==========================================

(define parsed-function
  (parse test-function))


(displayln "Parsed result:")

(displayln parsed-function)

(newline)


; ==========================================
; EXECUTE
; ==========================================

(displayln "Execution result:")

(displayln
 (process parsed-function))


(newline)


; ==========================================
; SHOW ENVIRONMENT
; ==========================================

(displayln "Environment after function:")

(displayln environment)
