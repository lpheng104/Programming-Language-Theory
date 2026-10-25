#lang racket

; ==========================================
; ENVIRONMENT
; ==========================================

(define scope
  (list
   (list 'a 1)
   (list 'b 2)))

(define environment (list scope))

; ==========================================
; VARIABLE LOOKUP
; ==========================================

(define (resolve var_scope var_name)
  (cond
    [(null? var_scope) (void)]
    [(eq? (caar var_scope) var_name)
     (cadar var_scope)]
    [else
     (resolve (cdr var_scope) var_name)]))

(define (resolve_env var_env var_name)
  (cond
    [(null? var_env) (void)]
    [(void? (resolve (car var_env) var_name))
     (resolve_env (cdr var_env) var_name)]
    [else
     (resolve (car var_env) var_name)]))

; ==========================================
; VARIABLE ASSIGNMENT
; ==========================================

(define (update_pair left_part lst key value)
  (cond
    [(null? lst) left_part]
    [(eq? (caar lst) key)
     (append left_part
             (list (list key value))
             (cdr lst))]
    [else
     (update_pair
      (append left_part (list (car lst)))
      (cdr lst)
      key
      value)]))

(define (update_scope var_scope var_name var_value)
  (cond
    [(void? (resolve var_scope var_name))
     (cons (list var_name var_value) var_scope)]
    [else
     (update_pair '()
                  var_scope
                  var_name
                  var_value)]))

; Update the nearest existing variable.
; If it does not exist, insert into the top scope.

(define (wolf_env var_env var_name var_value)

  (define (update-existing env)
    (cond
      [(null? env) '()]
      [(not (void? (resolve (car env) var_name)))
       (cons
        (update_scope (car env) var_name var_value)
        (cdr env))]
      [else
       (cons (car env)
             (update-existing (cdr env)))]))

  (cond
    [(null? var_env)
     (error "ERROR: environment is empty")]

    [(void? (resolve_env var_env var_name))
     (set! environment
           (cons
            (cons (list var_name var_value)
                  (car var_env))
            (cdr var_env)))]

    [else
     (set! environment
           (update-existing var_env))]))

; ==========================================
; SCOPE MANAGEMENT
; ==========================================

(define (push_scope new_scope)
  (set! environment
        (cons new_scope environment)))

(define (pop_scope)
  (if (null? environment)
      (error "ERROR: empty environment")
      (set! environment (cdr environment))))

(define (update_base_environment new_env)
  (set! environment new_env))

; ==========================================
; MATH
; ==========================================

(define (is_valid_math_op op)
  (if (member op '(+ - * / // %))
      #t
      #f))

(define (do_math op left right)
  (cond
    [(eq? op '+) (+ left right)]
    [(eq? op '-) (- left right)]
    [(eq? op '*) (* left right)]
    [(eq? op '/) (/ left right)]
    [(eq? op '//) (quotient left right)]
    [(eq? op '%) (modulo left right)]
    [else (error "ERROR: invalid math operator")]))

; ==========================================
; BOOLEAN OPERATORS
; ==========================================

(define (is_valid_boolean_op op)
  (if (member op '(> >= < <= == && || and or))
      #t
      #f))

(define (calculate_boolean op left right)
  (cond
    [(eq? op '>) (> left right)]
    [(eq? op '>=) (>= left right)]
    [(eq? op '<) (< left right)]
    [(eq? op '<=) (<= left right)]
    [(eq? op '==) (equal? left right)]
    [(or (eq? op '&&) (eq? op 'and))
     (and left right)]
    [(or (eq? op '||) (eq? op 'or))
     (or left right)]
    [else (error "ERROR: invalid boolean operator")]))

; ==========================================
; FUNCTION PARAMETER PAIRS
; ==========================================

(define (create_pair_list lst_ret lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      (reverse lst_ret)
      (create_pair_list
       (cons (list (car lst1) (car lst2))
             lst_ret)
       (cdr lst1)
       (cdr lst2))))

(provide (all-defined-out))
