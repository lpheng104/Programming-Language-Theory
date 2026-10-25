#lang racket

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

; ==========================================
; TEST HELPER
; ==========================================

(define (run-test title code)
  (displayln "================================")
  (displayln title)
  (displayln "================================")

  (displayln "Original:")
  (displayln code)

  (define parsed (parse code))

  (displayln "Parsed:")
  (displayln parsed)

  (displayln "Execution:")
  (displayln (process parsed))

  (displayln "Environment after execution:")
  (displayln environment)

  (newline))

; ==========================================
; TASK 1: IF / ASK CONDITION
; ==========================================

(run-test
 "TASK 1: ASK TRUE"
 '(ask
   (a < b)
   ((a + b))
   ((a * b))))

(run-test
 "TASK 1: ASK FALSE"
 '(ask
   (a > b)
   ((a + b))
   ((a * b))))

; ==========================================
; TASK 2: WHILE LOOP
; ==========================================

; Local a starts at 1.
; Increment until a reaches 10.

(run-test
 "TASK 2: WHILE LOOP"
 '(while
   (a < 10)
   ((set a (a + 1)))))

; ==========================================
; TASK 3: DO-WHILE LOOP
; ==========================================

(run-test
 "TASK 3: DO-WHILE LOOP"
 '(do
   ((set a (a + 1)))
   while
   (a < 10)))

; ==========================================
; EXTRA TEST: DO-WHILE RUNS ONCE
; ==========================================

; Condition is already false.
; Body must still execute one time.

(run-test
 "EXTRA: DO-WHILE RUNS AT LEAST ONCE"
 '(do
   ((set a (a + 1)))
   while
   (a < 0)))

; ==========================================
; EXTRA TEST: FUNCTION STILL WORKS
; ==========================================

(run-test
 "EXTRA: FUNCTION"
 '(function
   (x y)
   ((x * y))
   (4 5)))

(displayln "ALL TESTS COMPLETED")
