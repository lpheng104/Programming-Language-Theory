#lang racket

(require "util.rkt")

; Parse a list of statements
(define (parse-statements statements)
  (cons 'list-exp (map parse statements)))

; Main parser
(define (parse exp)
  (cond

    ; Boolean literals
    [(or (eq? exp 'True) (eq? exp #t))
     (list 'boolean-exp 'True)]

    [(or (eq? exp 'False) (eq? exp #f))
     (list 'boolean-exp 'False)]

    ; Variable
    [(symbol? exp)
     (list 'var-exp exp)]

    ; Tagged number
    [(number? exp)
     (list 'num-exp (list 'num exp))]

    ; Tagged string
    [(string? exp)
     (list 'string-exp (list 'str exp))]

    ; Invalid expression
    [(or (not (list? exp)) (null? exp))
     (error "PARSER ERROR: invalid expression.")]

    ; Main program
    [(eq? (car exp) 'main)
     (parse-statements (cdr exp))]

    ; Assignment
    [(and (= (length exp) 3)
          (eq? (cadr exp) '=))
     (list 'assign-exp
           (list
            (list 'var-exp (car exp))
            (parse (caddr exp))))]

    ; Named function definition
    [(eq? (car exp) 'func)
     (list
      'assign-exp
      (list
       (list 'var-exp (cadr exp))
       (list
        'new-func-exp
        (map parse (caddr exp))
        (parse-statements (cadddr exp)))))]

    ; Function call
    [(eq? (car exp) 'call)
     (list 'call-exp
           (list 'var-exp (cadr exp))
           (map parse (caddr exp)))]

    ; Return
    [(eq? (car exp) 'return)
     (list 'return-exp
           (parse (cadr exp)))]

    ; Anonymous function
    [(eq? (car exp) 'function)
     (list 'func-exp
           (create_pair_list
            '()
            (map parse (cadr exp))
            (map parse (cadddr exp)))
           (parse-statements (caddr exp)))]

    ; Ask / conditional
    [(eq? (car exp) 'ask)
     (list 'ask-exp
           (parse (cadr exp))
           (parse-statements (caddr exp))
           (parse-statements (cadddr exp)))]

    ; While loop
    [(eq? (car exp) 'while)
     (list 'while-exp
           (parse (cadr exp))
           (parse-statements (caddr exp)))]

    ; Do-while loop
    [(eq? (car exp) 'do-while)
     (list 'do-while-exp
           (parse-statements (cadr exp))
           (parse (caddr exp)))]

    ; For loop
    ; Initialization, condition, increment, body
    [(eq? (car exp) 'for)
     (list 'for-exp
           (parse-statements (cadr exp))
           (parse (caddr exp))
           (parse-statements (cadddr exp))
           (parse-statements (list-ref exp 4)))]

    ; Fire
    [(eq? (car exp) 'fire)
     (list 'fire-exp
           (parse (cadr exp)))]

    ; NOT operator
    [(and (= (length exp) 2)
          (eq? (car exp) '!))
     (list 'boolean-exp
           '!
           (parse (cadr exp)))]

    ; Math operators
    [(and (= (length exp) 3)
          (is_valid_math_op (cadr exp)))
     (list 'math-exp
           (cadr exp)
           (parse (car exp))
           (parse (caddr exp)))]

    ; Comparison and logic operators
    [(and (= (length exp) 3)
          (is_valid_boolean_op (cadr exp)))
     (list 'boolean-exp
           (cadr exp)
           (parse (car exp))
           (parse (caddr exp)))]

    [else
     (error
      "PARSER ERROR: unsupported statement.")]))

(provide (all-defined-out))
