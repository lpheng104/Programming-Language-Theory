#lang racket

(provide value-of)

; Search the environment for a variable
(define resolve
  (lambda (env var-name)
    (cond
      ((null? env)
       (error "variable not found"))

      ((eq? (car (car env)) var-name)
       (cadr (car env)))

      (else
       (resolve (cdr env) var-name)))))


; Interpret the parser's output
(define value-of
  (lambda (exp env)
    (cond

      ; If parser gave us (var-exp a)
      ((eq? (car exp) 'var-exp)
       (resolve env (cadr exp)))

      (else
       (error "unknown expression")))))
