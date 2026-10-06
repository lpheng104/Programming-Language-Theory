#lang racket

(require "util.rkt")


(define
  process
  (lambda (parsed-exp)

    (cond

      ; -------------------------
      ; EMPTY
      ; -------------------------

      ((null? parsed-exp)
       (displayln
        "ERROR: EMPTY PROGRAM."))


      ; parser returned an error
      ((void? parsed-exp)
       (void))


      ; -------------------------
      ; FUNCTION CALL
      ; -------------------------

      ; Parsed format:
      ;
      ; ((func-exp (...) (...))
      ;  (...arguments...))

      ((and
        (list? parsed-exp)
        (list? (car parsed-exp))
        (eq?
         'func-exp
         (car (car parsed-exp))))

       (let*
           (
            ; (func-exp parameters body)
            (function-exp
             (car parsed-exp))

            ; parsed argument expressions
            (arguments
             (cadr parsed-exp))

            ; parameter expressions
            (parameters
             (cadr function-exp))

            ; function body
            (body
             (caddr function-exp))

            ; Evaluate arguments
            ;
            ; ((num-exp 4) (num-exp 5))
            ; becomes
            ; (4 5)
            (values
             (map process arguments))

            ; create:
            ; ((a 4) (b 5))
            (new-scope
             (create_function_scope
              parameters
              values))
            )

         ; push the function's scope
         (push_scope new-scope)

         ; execute the body
         (let
             ((result
               (process body)))

           ; remove function scope
           (pop_scope)

           ; return body result
           result))
       )


      ; -------------------------
      ; VARIABLE
      ; -------------------------

      ((eq?
        'var-exp
        (car parsed-exp))

       (let
           ((value
             (resolve_env
              environment
              (cadr parsed-exp))))

         (if
          (void? value)

          (begin
            (displayln
             "ERROR: variable not found")
            (void))

          value)))


      ; -------------------------
      ; NUMBER
      ; -------------------------

      ((eq?
        'num-exp
        (car parsed-exp))

       (cadr parsed-exp))


      ; -------------------------
      ; STRING
      ; -------------------------

      ((eq?
        'string-exp
        (car parsed-exp))

       (cadr parsed-exp))


      ; -------------------------
      ; MATH
      ; -------------------------

      ((eq?
        'math-exp
        (car parsed-exp))

       (let
           (
            (left
             (process
              (caddr parsed-exp)))

            (right
             (process
              (cadddr parsed-exp)))
            )

         (if
          (and
           (number? left)
           (number? right))

          (do_math
           (cadr parsed-exp)
           left
           right)

          (displayln
           "INTERPRETER ERROR: non-numeric cannot apply math.")))
       )


      ; -------------------------
      ; UNKNOWN EXPRESSION
      ; -------------------------

      (else
       (displayln
        "ERROR: expression has not been supported yet."))

      )
    )
  )

(provide (all-defined-out))
