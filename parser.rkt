#lang racket

(require "util.rkt")

; ==========================================
; PARSER
; ==========================================

(define (parse exp)
  (cond

    ; BOOLEAN LITERALS
    [(boolean? exp)
     (list 'boolean-exp
           (if exp 'True 'False)
           (void)
           (void))]

    [(and (symbol? exp)
          (memq exp '(True False)))
     (list 'boolean-exp exp (void) (void))]

    ; VARIABLE
    [(symbol? exp)
     (list 'var-exp exp)]

    ; NUMBER
    [(number? exp)
     (list 'num-exp exp)]

    ; STRING
    [(string? exp)
     (list 'string-exp exp)]

    ; EMPTY
    [(null? exp)
     (error "PARSER ERROR: empty statement")]

    [(not (list? exp))
     (error "PARSER ERROR: unsupported input")]

    ; ======================================
    ; ASSIGNMENT
    ;
    ; (a = 1)
    ; ->
    ; (assign-exp (var-exp a) (num-exp 1))
    ; ======================================

    [(and (= (length exp) 3)
          (eq? (cadr exp) '=))

     (unless (symbol? (car exp))
       (error "PARSER ERROR: invalid assignment variable"))

     (list 'assign-exp
           (parse (car exp))
           (parse (caddr exp)))]

    ; ======================================
    ; OLD SET SYNTAX
    ; (set a 1)
    ; ======================================

    [(eq? (car exp) 'set)
     (unless (and (= (length exp) 3)
                  (symbol? (cadr exp)))
       (error "PARSER ERROR: invalid set expression"))

     (list 'assign-exp
           (parse (cadr exp))
           (parse (caddr exp)))]

    ; ======================================
    ; FOR LOOP
    ;
    ; (for
    ;   (a = 0)
    ;   (a < 10)
    ;   (a = (a + 1))
    ;   ((b = (b + 1))))
    ; ======================================

    [(eq? (car exp) 'for)
     (unless (= (length exp) 5)
       (error "PARSER ERROR: invalid for loop"))

     (list
      'for-exp
      (parse (cadr exp))
      (parse (caddr exp))
      (parse (cadddr exp))
      (parse-bulk (list-ref exp 4)))]

    ; ======================================
    ; WHILE LOOP
    ; ======================================

    [(eq? (car exp) 'while)
     (unless (= (length exp) 3)
       (error "PARSER ERROR: invalid while loop"))

     (list
      'while-exp
      (parse (cadr exp))
      (parse-bulk (caddr exp)))]

    ; ======================================
    ; DO-WHILE LOOP
    ; ======================================

    [(eq? (car exp) 'do)
     (unless (and (= (length exp) 4)
                  (eq? (caddr exp) 'while))
       (error "PARSER ERROR: invalid do-while"))

     (list
      'do-while-exp
      (parse-bulk (cadr exp))
      (parse (cadddr exp)))]

    ; ======================================
    ; ASK / IF CONDITION
    ; ======================================

    [(eq? (car exp) 'ask)
     (unless (= (length exp) 4)
       (error "PARSER ERROR: invalid ask"))

     (list
      'ask-exp
      (parse (cadr exp))
      (parse-bulk (caddr exp))
      (parse-bulk (cadddr exp)))]

    ; ======================================
    ; FUNCTION
    ; ======================================

    [(eq? (car exp) 'function)
     (unless
         (and (= (length exp) 4)
              (list? (cadr exp))
              (list? (caddr exp))
              (list? (cadddr exp))
              (= (length (cadr exp))
                 (length (cadddr exp)))
              (not (null? (caddr exp))))
       (error "PARSER ERROR: invalid function"))

     (list
      'func-exp
      (create_pair_list
       '()
       (map parse (cadr exp))
       (map parse (cadddr exp)))
      (parse-bulk (caddr exp)))]

    ; ======================================
    ; NOT
    ; ======================================

    [(and (= (length exp) 2)
          (eq? (car exp) '!))
     (list 'boolean-exp
           '!
           (parse (cadr exp))
           (void))]

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

    [else
     (error "PARSER ERROR: unsupported expression")]))

; ==========================================
; BULK PARSER
; ==========================================

(define (parse-bulk statements)
  (unless (list? statements)
    (error "PARSER ERROR: body must be a list"))

  (cons 'bulk-exp
        (map parse statements)))

(provide (all-defined-out))
