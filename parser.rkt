#lang racket

; parser converts our language into a temporary expression form
; A symbol is treated as a variable:
; a -> (var-exp a)

(define
  parse
  (lambda (exp)
    (cond
      ; If the input is a symbol, convert it to a var-exp
      ((symbol? exp)
       (list 'var-exp exp))

      ; Anything else is currently unsupported
      (else
       (displayln "ERROR"))
      )
    )
  )

(provide (all-defined-out))
