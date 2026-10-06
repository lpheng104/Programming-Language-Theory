#lang racket

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(displayln "=== Interpreter Tests ===")

; Test 1: variable a exists
(display "Test a: ")
(displayln (execute (parse 'a)))
; Expected: 1

; Test 2: variable b exists
(display "Test b: ")
(displayln (execute (parse 'b)))
; Expected: 2

; Test 3: variable c does not exist
(display "Test c: ")
(execute (parse 'c))
; Expected: ERROR: variable not found
