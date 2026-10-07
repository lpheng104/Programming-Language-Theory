#lang racket

(require "util.rkt")


; Parser translates the programming language
; into an intermediate form.

(define
  parse
  (lambda (exp)

    (cond

      ; ======================================
      ; VARIABLE
      ;
      ; a
      ; ->
      ; (var-exp a)
      ; ======================================

      ((symbol? exp)
       (list
        'var-exp
        exp))


      ; ======================================
      ; NUMBER
      ;
      ; 10
      ; ->
      ; (num-exp 10)
      ; ======================================

      ((number? exp)
       (list
        'num-exp
        exp))


      ; ======================================
      ; STRING
      ; ======================================

      ((string? exp)
       (list
        'string-exp
        exp))


      ; ======================================
      ; EMPTY
      ; ======================================

      ((null? exp)
       (displayln
        "ERROR: empty statement"))


      ; ======================================
      ; NOT
      ;
      ; (! a)
      ;
      ; ->
      ;
      ; (boolean-exp ! (var-exp a))
      ; ======================================

      ((and
        (list? exp)
        (= (length exp) 2)
        (eq? '! (car exp)))

       (list
        'boolean-exp
        '!
        (parse
         (cadr exp)))
       )


      ; ======================================
      ; BOOLEAN EXPRESSION
      ;
      ; (a > b)
      ;
      ; ->
      ;
      ; (boolean-exp
      ;     >
      ;     (var-exp a)
      ;     (var-exp b))
      ; ======================================

      ((and
        (list? exp)
        (= (length exp) 3)
        (is_valid_boolean_op
         (cadr exp)))

       (list
        'boolean-exp
        (cadr exp)
        (parse
         (car exp))
        (parse
         (caddr exp)))
       )


      ; ======================================
      ; MATH EXPRESSION
      ;
      ; (a + b)
      ;
      ; ->
      ;
      ; (math-exp
      ;     +
      ;     (var-exp a)
      ;     (var-exp b))
      ; ======================================

      ((and
        (list? exp)
        (= (length exp) 3)
        (is_valid_math_op
         (cadr exp)))

       (list
        'math-exp
        (cadr exp)
        (parse
         (car exp))
        (parse
         (caddr exp)))
       )


      ; ======================================
      ; ASK EXPRESSION
      ;
      ; (ask
      ;    (a < b)
      ;    ((a + b))
      ;    ((a * b)))
      ;
      ; ->
      ;
      ; (ask-exp
      ;    (boolean-exp ...)
      ;    (bulk-exp ...)
      ;    (bulk-exp ...))
      ; ======================================

      ((and
        (list? exp)
        (= (length exp) 4)
        (eq? 'ask (car exp)))

       (list
        'ask-exp

        ; Boolean condition
        (parse
         (cadr exp))

        ; True statements
        (cons
         'bulk-exp
         (map
          parse
          (caddr exp)))

        ; False statements
        (cons
         'bulk-exp
         (map
          parse
          (cadddr exp)))
        )
       )


      ; ======================================
      ; FUNCTION EXPRESSION
      ; ======================================

      ((and
        (list? exp)
        (eq? 'function (car exp)))

       (if
        (and
         (equal?
          (length (cadr exp))
          (length (cadddr exp)))

         (not
          (null? (caddr exp))))

        (list
         'func-exp

         (create_pair_list
          '()
          (map
           parse
           (cadr exp))
          (map
           parse
           (cadddr exp)))

         (cons
          'bulk-exp
          (map
           parse
           (caddr exp)))
         )

        (displayln
         "PARSER ERROR: this is not a valid anonymous function definition."))
       )


      ; ======================================
      ; UNSUPPORTED
      ; ======================================

      (else
       (displayln
        "PARSER ERROR: the statement has not been supported yet."))
      )
    )
  )


(provide (all-defined-out))
