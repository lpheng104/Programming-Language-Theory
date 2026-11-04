#lang racket

(require "parser.rkt")
(require "interpreter.rkt")
(require "util.rkt")

; ==========================================
; RESET ENVIRONMENT
; ==========================================

(define (reset-environment)
  (update_base_environment
   (list '((a 1) (b 2)))))

; ==========================================
; TEST HELPER
; ==========================================

(define (run-test test-name program)

  (reset-environment)

  (displayln "====================================")
  (displayln test-name)
  (displayln "====================================")

  (displayln "Program:")
  (write program)
  (newline)

  (displayln "Output:")

  (with-handlers
      ([exn:fail?
        (lambda (error-value)
          (displayln (exn-message error-value)))])

    (process (parse program)))

  (displayln "Environment:")
  (write environment)
  (newline)
  (newline))

; ==========================================
; T1: MAIN PROGRAM
;
; Expected output:
; 10
; ==========================================

(run-test
 "T1: MAIN PROGRAM"
 '(main
   ((a = 5)
    (b = (a * 2))
    (fire b))))

; ==========================================
; T2: INVALID MAIN SYNTAX
;
; Expected:
; PARSER ERROR: main must hold
; a list of statements.
; ==========================================

(run-test
 "T2: INVALID MAIN SYNTAX"
 '(main (a = 1)))

; ==========================================
; T3: FUNCTION DEFINITION AND SCOPE
;
; Expected:
; 2500
; 1
; #<void>
; ==========================================

(run-test
 "T3: FUNCTION DEFINITION AND SCOPE"
 '(main
   ((func sum_seq (a b s)
      ((c = (((a + b) * (((b - a) / s) + 1)) / 2))
       (return c)))

    (x = (call sum_seq (1 99 2)))

    (fire x)
    (fire a)
    (fire c))))

; ==========================================
; T4: FUNCTION WITHOUT RETURN
;
; Expected:
; 7
; #<void>
; ==========================================

(run-test
 "T4: FUNCTION WITHOUT RETURN"
 '(main
   ((func hello (n)
      ((fire n)))

    (y = (call hello (7)))

    (fire y))))

; ==========================================
; T5: EARLY RETURN
;
; Expected:
; 3
;
; 999 must NOT print.
; ==========================================

(run-test
 "T5: EARLY RETURN"
 '(main
   ((func first (n)
      ((return n)
       (fire 999)))

    (fire (call first (3))))))

; ==========================================
; T6: WRONG NUMBER OF ARGUMENTS
;
; Expected:
; INTERPRETER ERROR:
; wrong number of arguments.
; ==========================================

(run-test
 "T6: WRONG NUMBER OF ARGUMENTS"
 '(main
   ((func sum_seq (a b s)
      ((return a)))

    (call sum_seq (1 2)))))

; ==========================================
; T7: CALLING A NON-FUNCTION
;
; Expected:
; INTERPRETER ERROR: not a function.
; ==========================================

(run-test
 "T7: CALLING A NON-FUNCTION"
 '(main
   ((q = 5)
    (call q (1)))))

; ==========================================
; T8: ARGUMENT EVALUATION AND SHADOWING
;
; Expected:
; 21
; 10
; ==========================================

(run-test
 "T8: ARGUMENT EVALUATION AND SHADOWING"
 '(main
   ((k = 10)

    (func addone (k)
      ((return (k + 1))))

    (fire (call addone ((k * 2))))

    (fire k))))

; ==========================================
; BONUS B1: RECURSIVE FACTORIAL
;
; Expected:
; 120
; ==========================================

(run-test
 "B1: RECURSIVE FACTORIAL"
 '(main
   ((func fact (n)
      ((ask (n < 2)
         ((return 1))
         ((return
           (n * (call fact ((n - 1)))))))))

    (fire (call fact (5))))))

; ==========================================
; BONUS B2: RETURN INSIDE WHILE LOOP
;
; Expected:
; 40
; -1
; ==========================================

(run-test
 "B2: RETURN INSIDE WHILE LOOP"
 '(main
   ((func find (lim)
      ((i = 0)

       (while (i < 100)
         ((i = (i + 1))

          (ask (i == lim)
            ((return (i * 10)))
            ((i = i)))))

       (return -1)))

    (fire (call find (4)))
    (fire (call find (500))))))

; ==========================================
; FINISHED
; ==========================================

(displayln "====================================")
(displayln "ALL TESTS COMPLETED")
(displayln "====================================")
