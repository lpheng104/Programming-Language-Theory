#lang racket

; ---------------------------------
; GLOBAL SCOPE
; ---------------------------------

(define
  scope
  (list
   (list 'a 1)
   (list 'b 2)
   ))

; environment is a stack of scopes
(define
  environment
  (list scope)
  )


; ---------------------------------
; VARIABLE LOOKUP
; ---------------------------------

; search one scope
(define
  resolve
  (lambda (var_scope var_name)

    (cond

      ((null? var_scope)
       (void))

      ((eq? (car (car var_scope)) var_name)
       (cadr (car var_scope)))

      (else
       (resolve
        (cdr var_scope)
        var_name))
      )
    )
  )


; search the whole environment
(define
  resolve_env
  (lambda (var_env var_name)

    (cond

      ((null? var_env)
       (void))

      ((void?
        (resolve
         (car var_env)
         var_name))

       (resolve_env
        (cdr var_env)
        var_name))

      (else
       (resolve
        (car var_env)
        var_name))
      )
    )
  )


; ---------------------------------
; FUNCTION SCOPE UTILITIES
; ---------------------------------

; Push a new scope onto the
; environment stack
(define
  push_scope
  (lambda (new_scope)

    (set!
     environment
     (cons
      new_scope
      environment))
    )
  )


; Remove the top scope when
; the function finishes
(define
  pop_scope
  (lambda ()

    (cond

      ((null? environment)
       (displayln
        "ERROR: environment is empty"))

      (else
       (set!
        environment
        (cdr environment)))
      )
    )
  )


; Create a function scope from
; parameters and values
;
; ((var-exp a) (var-exp b))
; and
; (4 5)
;
; becomes
;
; ((a 4) (b 5))

(define
  create_function_scope
  (lambda (parameters values)

    (cond

      ((and
        (null? parameters)
        (null? values))
       '())

      ((or
        (null? parameters)
        (null? values))

       (error
        "function parameter count does not match argument count"))

      (else

       (cons

        (list
         (cadr (car parameters))
         (car values))

        (create_function_scope
         (cdr parameters)
         (cdr values))))
      )
    )
  )


; ---------------------------------
; MATH
; ---------------------------------

(define
  do_math
  (lambda
      (op left_operand right_operand)

    (cond

      ((eq? '+ op)
       (+ left_operand right_operand))

      ((eq? '- op)
       (- left_operand right_operand))

      ((eq? '* op)
       (* left_operand right_operand))

      ((eq? '/ op)
       (/ left_operand right_operand))

      ((eq? '// op)
       (quotient
        left_operand
        right_operand))

      ((eq? '% op)
       (modulo
        left_operand
        right_operand))

      (else
       (displayln
        "ERROR: unsupported math operator"))
      )
    )
  )

(provide (all-defined-out))
