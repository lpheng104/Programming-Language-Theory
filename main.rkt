#lang racket

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")


(displayln "============================")
(displayln "FUNCTION INTERPRETER TEST")
(displayln "============================")
(newline)


; Our language code:
;
; (function (params a b)
;           (math a + b))
; (4 5)

(define
  function-test
  '((function
      (params a b)
      (math a + b))
    (4 5)))


; -----------------------------
; PARSER TEST
; -----------------------------

(displayln "Original code:")
(displayln function-test)

(newline)

(displayln "Parsed result:")

(define
  parsed-function
  (parse function-test))

(displayln parsed-function)


; -----------------------------
; INTERPRETER TEST
; -----------------------------

(newline)

(displayln "Execution result:")

(displayln
 (process parsed-function))


; -----------------------------
; SHOW ENVIRONMENT CLEANUP
; -----------------------------

(newline)

(displayln "Environment after function:")
(displayln environment)
