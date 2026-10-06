#lang racket

; A scope contains key-value pairs
; Example:
; (a 1)
; (b 2)

(define
  scope
  (list
   (list 'a 1)
   (list 'b 2)
   )
  )

; The environment is a list of scopes
(define
  environment
  (list scope)
  )


; Searches one scope for a variable
(define
  resolve_scope
  (lambda (scope variable)
    (cond
      ; If there are no more variables in this scope,
      ; return void to indicate it was not found
      ((null? scope)
       (void))

      ; If the first variable matches,
      ; return its value
      ((eq? (car (car scope)) variable)
       (cadr (car scope)))

      ; Otherwise continue searching
      (else
       (resolve_scope (cdr scope) variable))
      )
    )
  )


; Searches through all scopes in the environment
(define
  resolve_env
  (lambda (env variable)
    (cond
      ; No scopes left, variable was not found
      ((null? env)
       (void))

      ; Search the first scope
      (else
       (let ((value (resolve_scope (car env) variable)))
         (if (void? value)

             ; Not found in this scope,
             ; search the next scope
             (resolve_env (cdr env) variable)

             ; Found it
             value)))
      )
    )
  )

(provide (all-defined-out))
