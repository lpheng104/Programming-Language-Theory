#lang racket

(require "util.rkt")

; ==========================================
; PARSE A LIST OF STATEMENTS
; ==========================================

(define (parse-statements statements)
  (cons 'list-exp
        (map parse statements)))

(define (valid-statement-list? value)
  (and (list? value)
       (andmap list? value)))

; ==========================================
; MAIN PARSER
; ==========================================

(define (parse exp)
  (cond

    ; ======================================
    ; BOOLEAN LITERALS
    ; ======================================

    [(boolean? exp)
     (list 'boolean-exp
           (if exp 'True 'False))]

    [(and (symbol? exp)
          (memq exp '(True False)))
     (list 'boolean-exp exp)]

    ; ======================================
    ; VARIABLE
    ; ======================================

    [(symbol? exp)
     (list 'var-exp exp)]

    ; ======================================
    ; NUMBER
    ; ======================================

    [(number? exp)
     (list 'num-exp exp)]

    ; ======================================
    ; STRING
    ; ======================================

    [(string? exp)
     (list 'string-exp exp)]

    ; ======================================
    ; EMPTY / INVALID
    ; ======================================

    [(null? exp)
     (error "PARSER ERROR: empty statement.")]

    [(not (list? exp))
     (error "PARSER ERROR: unsupported input.")]

    ; ======================================
    ; PART 1: MAIN PROGRAM
    ;
    ; (main ((a = 5) (fire a)))
    ; ->
    ; (list-exp ...)
    ; ======================================

    [(eq? (car exp) 'main)
     (unless
         (and (= (length exp) 2)
              (valid-statement-list? (cadr exp)))
       (error
        "PARSER ERROR: main must hold a list of statements."))

     (parse-statements (cadr exp))]

    ; ======================================
    ; PART 2: FUNCTION DEFINITION
    ;
    ; (func hello (n) ((fire n)))
    ; ======================================

    [(eq? (car exp) 'func)
     (unless
         (and (= (length exp) 4)
              (symbol? (cadr exp))
              (list? (caddr exp))
              (andmap symbol? (caddr exp))
              (valid-statement-list? (cadddr exp)))
       (error
        "PARSER ERROR: invalid function definition."))

     (list
      'func-def-exp
      (parse (cadr exp))
      (map parse (caddr exp))
      (parse-statements (cadddr exp)))]

    ; ======================================
    ; PART 2: FUNCTION CALL
    ;
    ; (call hello (7))
    ; ======================================

    [(eq? (car exp) 'call)
     (unless
         (and (= (length exp) 3)
              (symbol? (cadr exp))
              (list? (caddr exp)))
       (error
        "PARSER ERROR: invalid function call."))

     (list
      'call-exp
      (parse (cadr exp))
      (map parse (caddr exp)))]

    ; ======================================
    ; PART 2: RETURN
    ;
    ; (return n)
    ; ======================================

    [(eq? (car exp) 'return)
     (unless (= (length exp) 2)
       (error
        "PARSER ERROR: invalid return."))

     (list
      'return-exp
      (parse (cadr exp)))]

    ; ======================================
    ; FIRE / PRINT
    ;
    ; (fire a)
    ; ======================================

    [(eq? (car exp) 'fire)
     (unless (= (length exp) 2)
       (error "PARSER ERROR: invalid fire."))

     (list
      'fire-exp
      (parse (cadr exp)))]

    ; ======================================
    ; ASSIGNMENT
    ;
    ; (a = 5)
    ; ======================================

    [(and (= (length exp) 3)
          (eq? (cadr exp) '=))
     (unless (symbol? (car exp))
       (error
        "PARSER ERROR: invalid assignment."))

     (list
      'assign-exp
      (parse (car exp))
      (parse (caddr exp)))]

    ; ======================================
    ; OLD ASSIGNMENT SYNTAX
    ;
    ; (set a 5)
    ; ======================================

    [(eq? (car exp) 'set)
     (unless
         (and (= (length exp) 3)
              (symbol? (cadr exp)))
       (error
        "PARSER ERROR: invalid assignment."))

     (list
      'assign-exp
      (parse (cadr exp))
      (parse (caddr exp)))]

    ; ======================================
    ; FOR LOOP
    ; ======================================

    [(eq? (car exp) 'for)
     (unless
         (and (= (length exp) 5)
              (valid-statement-list?
               (list-ref exp 4)))
       (error
        "PARSER ERROR: invalid for loop."))

     (list
      'for-exp
      (parse (cadr exp))
      (parse (caddr exp))
      (parse (cadddr exp))
      (parse-statements (list-ref exp 4)))]

    ; ======================================
    ; WHILE LOOP
    ; ======================================

    [(eq? (car exp) 'while)
     (unless
         (and (= (length exp) 3)
              (valid-statement-list?
               (caddr exp)))
       (error
        "PARSER ERROR: invalid while loop."))

     (list
      'while-exp
      (parse (cadr exp))
      (parse-statements (caddr exp)))]

    ; ======================================
    ; DO-WHILE LOOP
    ; ======================================

    [(eq? (car exp) 'do)
     (unless
         (and (= (length exp) 4)
              (valid-statement-list? (cadr exp))
              (eq? (caddr exp) 'while))
       (error
        "PARSER ERROR: invalid do-while loop."))

     (list
      'do-while-exp
      (parse-statements (cadr exp))
      (parse (cadddr exp)))]

    ; ======================================
    ; ASK / IF CONDITION
    ; ======================================

    [(eq? (car exp) 'ask)
     (unless
         (and (= (length exp) 4)
              (valid-statement-list? (caddr exp))
              (valid-statement-list? (cadddr exp)))
       (error
        "PARSER ERROR: invalid ask."))

     (list
      'ask-exp
      (parse (cadr exp))
      (parse-statements (caddr exp))
      (parse-statements (cadddr exp)))]

    ; ======================================
    ; OLD ANONYMOUS FUNCTION
    ; ======================================

    [(eq? (car exp) 'function)
     (unless
         (and (= (length exp) 4)
              (list? (cadr exp))
              (andmap symbol? (cadr exp))
              (valid-statement-list? (caddr exp))
              (list? (cadddr exp))
              (= (length (cadr exp))
                 (length (cadddr exp))))
       (error
        "PARSER ERROR: invalid function."))

     (list
      'func-exp
      (map parse (cadr exp))
      (map parse (cadddr exp))
      (parse-statements (caddr exp)))]

    ; ======================================
    ; NOT
    ; ======================================

    [(and (= (length exp) 2)
          (eq? (car exp) '!))
     (list
      'boolean-exp
      '!
      (parse (cadr exp)))]

    ; ======================================
    ; BOOLEAN EXPRESSION
    ; ======================================

    [(and (= (length exp) 3)
          (is_valid_boolean_op (cadr exp)))
     (list
      'boolean-exp
      (cadr exp)
      (parse (car exp))
      (parse (caddr exp)))]

    ; ======================================
    ; MATH EXPRESSION
    ; ======================================

    [(and (= (length exp) 3)
          (is_valid_math_op (cadr exp)))
     (list
      'math-exp
      (cadr exp)
      (parse (car exp))
      (parse (caddr exp)))]

    ; ======================================
    ; UNSUPPORTED
    ; ======================================

    [else
     (error
      "PARSER ERROR: unsupported expression.")]))

; ==========================================
; COMPATIBILITY WITH OLD BULK PARSER
; ==========================================

(define (parse-bulk statements)
  (parse-statements statements))

(provide (all-defined-out))
