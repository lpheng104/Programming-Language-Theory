#lang racket

; ==========================================
; ENVIRONMENT
; ==========================================

; Global scope
(define
  scope
  (list
   (list 'a 1)
   (list 'b 2)
   )
  )

; Environment is a stack of scopes
(define
  environment
  (list scope)
  )


; ==========================================
; RESOLVE VARIABLE IN ONE SCOPE
; ==========================================

(define
  resolve
  (lambda (var_scope var_name)

    (cond

      ; Variable was not found
      ((null? var_scope)
       (void))

      ; Variable was found
      ((eq? (car (car var_scope)) var_name)
       (cadr (car var_scope)))

      ; Keep searching
      (else
       (resolve
        (cdr var_scope)
        var_name))
      )
    )
  )


; ==========================================
; RESOLVE VARIABLE IN ENVIRONMENT
; ==========================================

(define
  resolve_env
  (lambda (var_env var_name)

    (cond

      ; No more scopes
      ((null? var_env)
       (void))

      ; Variable not found in current scope
      ((void?
        (resolve
         (car var_env)
         var_name))

       ; Search next scope
       (resolve_env
        (cdr var_env)
        var_name))

      ; Variable was found
      (else
       (resolve
        (car var_env)
        var_name))
      )
    )
  )


; ==========================================
; PUSH A NEW SCOPE
; ==========================================

(define
  push_scope
  (lambda (new-scope)

    (set!
     environment
     (cons new-scope environment))
    )
  )


; ==========================================
; POP THE TOP SCOPE
; ==========================================

(define
  pop_scope
  (lambda ()

    (cond

      ((null? environment)
       (displayln "ERROR: environment is empty"))

      (else
       (set!
        environment
        (cdr environment)))
      )
    )
  )


; ==========================================
; MATH
; ==========================================

(define
  do_math
  (lambda (op left_operand right_operand)

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
       (quotient left_operand right_operand))

      ((eq? '% op)
       (modulo left_operand right_operand))

      (else
       (displayln
        "ERROR: unsupported math operator"))
      )
    )
  )


(provide (all-defined-out))
