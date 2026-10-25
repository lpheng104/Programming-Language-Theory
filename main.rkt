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

  (displayln "Parsed:")
  (displayln (parse code))

  (displayln "Execution:")
  (displayln (process (parse code)))

  (displayln "Environment:")
  (displayln environment)

  (newline))

; ==========================================
; TASK 1: UPDATE EXISTING VARIABLE
; ==========================================

(run-test
 "TASK 1: ASSIGN EXISTING VARIABLE"
 '(a = 1))

; ==========================================
; TASK 1: CREATE NEW VARIABLE
; ==========================================

(run-test
 "TASK 1: ASSIGN NEW VARIABLE"
 '(c = 5))

; ==========================================
; TASK 1: UPDATE NEW VARIABLE
; ==========================================

(run-test
 "TASK 1: UPDATE VARIABLE"
 '(c = (c + 5)))

; ==========================================
; TASK 2: FOR LOOP
;
; for (a = 0; a < 10; a = a + 1)
; {
;    b = b + 1;
; }
; ==========================================

(run-test
 "TASK 2: FOR LOOP"
 '(for
   (a = 0)
   (a < 10)
   (a = (a + 1))
   ((b = (b + 1)))))

; ==========================================
; VERIFY FINAL VARIABLE VALUES
; ==========================================

(displayln "================================")
(displayln "VERIFY FOR LOOP RESULTS")
(displayln "================================")

(displayln "Final value of a:")
(displayln (process (parse 'a)))

(displayln "Final value of b:")
(displayln (process (parse 'b)))

(newline)

; ==========================================
; EXTRA: ASK / IF CONDITION
; ==========================================

(run-test
 "EXTRA: ASK CONDITION"
 '(ask
   (a > b)
   ((a + b))
   ((a * b))))

; ==========================================
; EXTRA: FUNCTION
; ==========================================

(run-test
 "EXTRA: FUNCTION"
 '(function
   (x y)
   ((x * y))
   (4 5)))

(displayln "ALL TESTS COMPLETED")
