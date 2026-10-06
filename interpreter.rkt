#lang racket

(require "util.rkt")


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

      ((eq? 'var-exp (car parsed-exp))

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

          value))
       )


      ; ======================================
      ; NUMBER
      ; ======================================

      ((eq? 'num-exp (car parsed-exp))

       (cadr parsed-exp))


      ; ======================================
      ; MATH
      ; ======================================

      ((eq? 'math-exp (car parsed-exp))

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


      ; ======================================
      ; FUNCTION
      ; ======================================

      ((eq? 'func-exp (car parsed-exp))

       (let*
           (
            ; Get parameter/value pairs
            (pairs
             (cadr parsed-exp))

            ; Create a new scope.
            ;
            ; Example:
            ;
            ; ((var-exp a) (num-exp 4))
            ; ((var-exp b) (num-exp 5))
            ;
            ; becomes:
            ;
            ; ((a 4) (b 5))

            (new-scope

             (map
              (lambda (pair)

                (list

                 ; Get variable name
                 (cadr
                  (car pair))

                 ; Get parameter value
                 (process
                  (cadr pair)))

                )

              pairs))

            ; Get the bulk-exp
            (bulk
             (caddr parsed-exp))
            )


         ; ==================================
         ; PUSH NEW SCOPE
         ; ==================================

         (push_scope new-scope)


         ; ==================================
         ; EXECUTE FUNCTION BODY
         ; ==================================

         (let
             (
              (result

               (let loop
                   (
                    ; Remove 'bulk-exp
                    ; and get its statements
                    (statements
                     (cdr bulk))

                    (last-result
                     (void))
                    )

                 (if
                  (null? statements)

                  ; No statements left
                  (begin
                    last-result)

                  ; Execute next statement
                  (loop
                   (cdr statements)

                   (process
                    (car statements))))
                 ))
              )


           ; ==================================
           ; POP FUNCTION SCOPE
           ; ==================================

           (pop_scope)


           ; ==================================
           ; RETURN FUNCTION RESULT
           ; ==================================

           result)
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
