#lang racket

(require "util.rkt")


; Process the parser's results

(define
  process
  (lambda (parsed-exp)

    (cond

      ; ======================================
      ; EMPTY PROGRAM
      ; ======================================

      ((null? parsed-exp)
       (displayln
        "ERROR: EMPTY PROGRAM."))


      ; ======================================
      ; PARSER ERROR
      ; ======================================

      ((void? parsed-exp)
       (void))


      ; ======================================
      ; VARIABLE
      ; ======================================

      ((equal?
        'var-exp
        (car parsed-exp))

       (resolve_env
        environment
        (cadr parsed-exp))
       )


      ; ======================================
      ; NUMBER
      ; ======================================

      ((equal?
        'num-exp
        (car parsed-exp))

       (cadr parsed-exp)
       )


      ; ======================================
      ; STRING
      ; ======================================

      ((equal?
        'string-exp
        (car parsed-exp))

       (cadr parsed-exp)
       )


      ; ======================================
      ; MATH EXPRESSION
      ; ======================================

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

         (cond

           ((and
             (number? left)
             (number? right))

            (do_math
             (cadr parsed-exp)
             left
             right))

           (else
            (displayln
             "INTERPRETOR ERROR: non-numeric cannot apply math."))
           )
         )
       )


      ; ======================================
      ; BOOLEAN EXPRESSION
      ;
      ; (boolean-exp > left right)
      ;
      ; returns #t or #f
      ; ======================================

      ((eq?
        'boolean-exp
        (car parsed-exp))

       ; NOT only has one operand
       (if
        (eq?
         '!
         (cadr parsed-exp))

        ; Process (! value)
        (not
         (process
          (caddr parsed-exp)))

        ; All two-operand boolean expressions
        (let
            (
             (left
              (process
               (caddr parsed-exp)))

             (right
              (process
               (cadddr parsed-exp)))
             )

          (do_boolean
           (cadr parsed-exp)
           left
           right)
          )
        )
       )


      ; ======================================
      ; ASK EXPRESSION
      ;
      ; If condition is #t:
      ;     execute true bulk-exp
      ;
      ; If condition is #f:
      ;     execute false bulk-exp
      ; ======================================

      ((eq?
        'ask-exp
        (car parsed-exp))

       (if
        (process
         (cadr parsed-exp))

        ; TRUE
        (process
         (caddr parsed-exp))

        ; FALSE
        (process
         (cadddr parsed-exp))
        )
       )


      ; ======================================
      ; FUNCTION EXPRESSION
      ; ======================================

      ((eq?
        'func-exp
        (car parsed-exp))

       (let
           (
            ; Add parameter/value scope
            ; to the original environment
            (my_env

             (cons

              (map
               (lambda (pair)

                 (list
                  (cadr
                   (car pair))

                  (process
                   (cadr pair)))
                 )

               (cadr parsed-exp))

              environment)
             )

            ; Function body
            (expression_lst
             (caddr parsed-exp))

            ; Return value
            (ret_val
             (void))
            )

         (begin

           ; Push function scope
           (update_base_environment
            my_env)

           ; Process bulk-exp and get
           ; its returned value
           (set!
            ret_val

            (cadr
             (cadr
              (process
               expression_lst))))

           ; Pop function scope
           (update_base_environment
            (cdr environment))

           ; Return function result
           ret_val
           )
         )
       )


      ; ======================================
      ; BULK EXPRESSION
      ;
      ; Process every statement and return
      ; the result of the final statement.
      ; ======================================

      ((eq?
        (car parsed-exp)
        'bulk-exp)

       (if
        (null?
         (cdr parsed-exp))

        ; Empty bulk-exp
        (list
         'terminator-exp
         (list
          'bulk-ret
          (void)))

        ; Execute every statement
        (let
            (
             (ret_lst
              (map
               process
               (cdr parsed-exp)))
             )

          ; Return last result
          (list
           'terminator-exp

           (list
            'bulk-ret

            (list-ref
             ret_lst
             (-
              (length ret_lst)
              1)))
           )
          )
        )
       )


      ; ======================================
      ; UNSUPPORTED EXPRESSION
      ; ======================================

      (else
       (displayln
        "ERROR: expression has not been supported yet."))
      )
    )
  )


(provide (all-defined-out))
