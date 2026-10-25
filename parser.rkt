
#lang racket
(require "util.rkt")

(define (parse exp)
  (cond

    ; ======================================
    ; BASIC VALUES
    ; ======================================

    ((boolean? exp)
     (list 'boolean-exp
           (if exp 'True 'False)
           (void)
           (void)))

    ((and (symbol? exp)
          (memq exp '(True False)))
     (list 'boolean-exp exp (void) (void)))

    ((symbol? exp)
     (list 'var-exp exp))

    ((number? exp)
     (list 'num-exp exp))

    ((string? exp)
     (list 'string-exp exp))

    ((null? exp)
     (error "PARSER ERROR: empty statement"))

    ((not (list? exp))
     (error "PARSER ERROR: unsupported input"))

    ; ======================================
    ; ASSIGNMENT
    ;
    ; (set a (a + 1))
    ; ======================================

    ((eq? (car exp) 'set)
     (unless (and (= (length exp) 3)
                  (symbol? (cadr exp)))
       (error "PARSER ERROR: invalid set expression"))

     (list 'set-exp
           (cadr exp)
           (parse (caddr exp))))

    ; ======================================
    ; WHILE LOOP
    ;
    ; (while (a < 10)
    ;   ((set a (a + 1))))
    ; ======================================

    ((eq? (car exp) 'while)
     (unless (= (length exp) 3)
       (error "PARSER ERROR: invalid while loop"))

     (list
      'while-exp
      (parse (cadr exp))
      (parse-bulk (caddr exp))))

    ; ======================================
    ; DO-WHILE LOOP
    ;
    ; (do
    ;   ((set a (a + 1)))
    ;   while
    ;   (a < 10))
    ; ======================================

    ((eq? (car exp) 'do)
     (unless (and (= (length exp) 4)
                  (eq? (caddr exp) 'while))
       (error "PARSER ERROR: invalid do-while"))

     (list
      'do-while-exp
      (parse-bulk (cadr exp))
      (parse (cadddr exp))))

    ; ======================================
    ; ASK / IF CONDITION
    ; ======================================

    ((eq? (car exp) 'ask)
     (unless (= (length exp) 4)
       (error "PARSER ERROR: invalid ask"))

     (list
      'ask-exp
      (parse (cadr exp))
      (parse-bulk (caddr exp))
      (parse-bulk (cadddr exp))))

    ; ======================================
    ; FUNCTION
    ; ======================================

    ((eq? (car exp) 'function)
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
      (parse-bulk (caddr exp))))

    ; ======================================
    ; NOT
    ; ======================================

    ((and (= (length exp) 2)
          (eq? (car exp) '!))
     (list 'boolean-exp
           '!
           (parse (cadr exp))
           (void)))

    ; ======================================
    ; BOOLEAN EXPRESSION
    ; ======================================

    ((and (= (length exp) 3)
          (is_valid_boolean_op (cadr exp)))
     (list
      'boolean-exp
      (cadr exp)
      (parse (car exp))
      (parse (caddr exp))))

    ; ======================================
    ; MATH EXPRESSION
    ; ======================================

    ((and (= (length exp) 3)
          (is_valid_math_op (cadr exp)))
     (list
      'math-exp
      (cadr exp)
      (parse (car exp))
      (parse (caddr exp))))

    (else
     (error "PARSER ERROR: unsupported expression"))))

; ==========================================
; BULK PARSER
; ==========================================

(define (parse-bulk statements)
  (unless (list? statements)
    (error "PARSER ERROR: body must be a list"))

  (cons 'bulk-exp
        (map parse statements)))

(provide (all-defined-out))
