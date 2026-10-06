#lang racket

; parser translates our programming language
; into an intermediate representation

(define parse
  (lambda (exp)
    (cond

      ; variable
      ((symbol? exp)
       (list 'var-exp exp))

      ; number
      ((number? exp)
       (list 'num-exp exp))

      ; string
      ((string? exp)
       (list 'string-exp exp))

      ; empty statement
      ((null? exp)
       (displayln "ERROR: empty statement"))

      ; math expression
      ; (math a + b)
      ; ->
      ; (math-exp + (var-exp a) (var-exp b))
      ((and (list? exp)
            (eq? 'math (car exp)))

       (list
        'math-exp
        (caddr exp)
        (parse (cadr exp))
        (parse (cadddr exp))))

      ; function
      ;
      ; ((function (params a b)
      ;            (math a + b))
      ;   (4 5))
      ;
      ; ->
      ;
      ; ((func-exp
      ;    ((var-exp a) (var-exp b))
      ;    (math-exp + (var-exp a) (var-exp b)))
      ;  ((num-exp 4) (num-exp 5)))

      ((and (list? exp)
            (= (length exp) 2)
            (list? (car exp))
            (not (null? (car exp)))
            (eq? 'function (car (car exp))))

       (let*
           (
            ; (function (params a b) (math a + b))
            (function-definition (car exp))

            ; (4 5)
            (argument-list (cadr exp))

            ; (params a b)
            (parameter-section
             (cadr function-definition))

            ; (a b)
            (parameters
             (cdr parameter-section))

            ; (math a + b)
            (body
             (caddr function-definition))
            )

         (list

          ; function expression
          (list
           'func-exp
           (map parse parameters)
           (parse body))

          ; argument values
          (map parse argument-list))))

      ; anything else
      (else
       (displayln
        "PARSER ERROR: the statement has not been supported yet."))
      )
    )
  )

(provide (all-defined-out))
