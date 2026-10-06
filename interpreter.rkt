#lang racket

(require "util.rkt")

(define
  process
  (lambda (parsed-exp)
    (
     cond

     ((null? parsed-exp)
      (displayln "ERROR: EMPTY PROGRAM."))

     ; error handler from parser
     ((void? parsed-exp)
      (void))

     ; variable
     ((equal? 'var-exp (car parsed-exp))
      (resolve_env environment (cadr parsed-exp)))

     ; number
     ((equal? 'num-exp (car parsed-exp))
      (car (cdr parsed-exp)))

     ; math
     ((eq? 'math-exp (car parsed-exp))

      (cond

        ((and
          (number? (process (caddr parsed-exp)))
          (number? (process (cadddr parsed-exp))))

         (do_math
          (cadr parsed-exp)
          (process (caddr parsed-exp))
          (process (cadddr parsed-exp))))

        (else
         (displayln
          "INTERPRETOR ERROR: non-numeric cannot apply math."))
        )
      )

     ; ====================================
     ; FUNCTION
     ; ====================================

     ((eq? 'func-exp (car parsed-exp))

      (let*
          (
           ; Parameter/value pairs
           (pairs (cadr parsed-exp))

           ; Create the function scope
           (new-scope
            (map
             (lambda (pair)
               (list
                (cadr (car pair))
                (process (cadr pair))))
             pairs))

           ; Function body
           (bulk (caddr parsed-exp))
           )

        ; -----------------------------
        ; PUSH SCOPE
        ; -----------------------------

        (set! environment
              (cons new-scope environment))


        ; -----------------------------
        ; EXECUTE BODY
        ; -----------------------------

        (let
            ((result

              (let loop
                  ((statements (cdr bulk))
                   (last-result (void)))

                (if
                 (null? statements)

                 last-result

                 (loop
                  (cdr statements)
                  (process
                   (car statements)))))))

          ; -----------------------------
          ; POP SCOPE
          ; -----------------------------

          (set! environment
                (cdr environment))

          ; Return result
          result))
      )


     ; unsupported expression
     (else
      (displayln
       "ERROR: expression has not been supported yet."))

     ))
  )

(provide (all-defined-out))
