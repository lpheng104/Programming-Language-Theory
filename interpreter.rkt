#lang racket
(require "util.rkt")

; Takes the parser's output and executes it.
; Currently only var-exp expressions are supported.
(define
  execute
  (lambda (exp)
    (cond
      ; Check whether this is a var-exp
      ((and (list? exp)
            (= (length exp) 2)
            (eq? (car exp) 'var-exp))

       ; Look up the variable in the environment
       (let ((value (resolve_env environment (cadr exp))))
         (if (void? value)
             (displayln "ERROR: variable not found")
             value)))

      ; Any other expression is currently unsupported
      (else
       (displayln "ERROR: unsupported expression"))
      )
    )
  )

(provide (all-defined-out))
