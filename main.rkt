#lang racket

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")


(displayln "================================")
(displayln "BOOLEAN AND ASK TESTS")
(displayln "================================")

(newline)


; ==========================================
; TEST 1
; BOOLEAN: a > b
;
; a = 1
; b = 2
;
; Expected: #f
; ==========================================

(displayln "TEST 1: BOOLEAN")

(define boolean-test
  '(a > b))

(displayln "Original:")
(displayln boolean-test)

(displayln "Parsed:")
(displayln
 (parse boolean-test))

(displayln "Execution:")
(displayln
 (process
  (parse boolean-test)))

(newline)


; ==========================================
; TEST 2
; BOOLEAN: a < b
;
; Expected: #t
; ==========================================

(displayln "TEST 2: BOOLEAN")

(define boolean-test-2
  '(a < b))

(displayln "Original:")
(displayln boolean-test-2)

(displayln "Parsed:")
(displayln
 (parse boolean-test-2))

(displayln "Execution:")
(displayln
 (process
  (parse boolean-test-2)))

(newline)


; ==========================================
; TEST 3
; ASK TRUE
;
; a < b = TRUE
;
; TRUE:
;     a + b = 3
;
; FALSE:
;     a * b = 2
;
; Expected branch result = 3
; ==========================================

(displayln "TEST 3: ASK TRUE")

(define ask-true-test

  '(ask
    (a < b)

    ; true statements
    ((a + b))

    ; false statements
    ((a * b)))
  )


(displayln "Original:")
(displayln ask-true-test)

(displayln "Parsed:")
(displayln
 (parse ask-true-test))

(displayln "Execution:")
(displayln
 (process
  (parse ask-true-test)))

(newline)


; ==========================================
; TEST 4
; ASK FALSE
;
; a > b = FALSE
;
; TRUE:
;     a + b = 3
;
; FALSE:
;     a * b = 2
;
; Expected branch result = 2
; ==========================================

(displayln "TEST 4: ASK FALSE")

(define ask-false-test

  '(ask
    (a > b)

    ; true statements
    ((a + b))

    ; false statements
    ((a * b)))
  )


(displayln "Original:")
(displayln ask-false-test)

(displayln "Parsed:")
(displayln
 (parse ask-false-test))

(displayln "Execution:")
(displayln
 (process
  (parse ask-false-test)))

(newline)


; ==========================================
; TEST 5
; FUNCTION FROM PREVIOUS ASSIGNMENT
;
; Make sure functions still work.
; ==========================================

(displayln "TEST 5: FUNCTION")

(define function-test

  '(function
    (a b)

    ((1 + 1)
     (a * b)
     (a + 1))

    (4 5))
  )


(displayln "Parsed:")
(displayln
 (parse function-test))

(displayln "Execution:")
(displayln
 (process
  (parse function-test)))
