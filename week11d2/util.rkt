#lang racket

; ==========================================
; TAGGED VALUES
; ==========================================

(define (make_num value)
  (list 'num value))

(define (make_str value)
  (list 'str value))

(define (make_bool value)
  (list 'bool value))

(define (make_void)
  (list 'void (void)))

(define (tagged_value? value)
  (and (list? value)
       (= (length value) 2)
       (memq (car value)
             '(num str bool func sym cls obj void none))
       #t))

; Check the type and return the plain value
(define (check_type type pair)
  (if (and (tagged_value? pair)
           (eq? type (car pair)))
      (cadr pair)
      (error
       (string-append
        "Data type error: expect "
        (symbol->string type)
        "."))))

; ==========================================
; STARTING ENVIRONMENT
; ==========================================

(define scope
  (list
   (list 'a (make_num 1))
   (list 'b (make_num 2))
   (list 'c (make_str "ccc"))
   (list 'e (make_bool #t))
   (list 'f (make_bool #f))))

(define environment
  (list scope))

; ==========================================
; VARIABLE LOOKUP
; ==========================================

(define (resolve var_scope var_name)
  (cond
    [(null? var_scope)
     (void)]

    [(eq? (caar var_scope) var_name)
     (cadar var_scope)]

    [else
     (resolve
      (cdr var_scope)
      var_name)]))

(define (resolve_env var_env var_name)
  (cond
    [(null? var_env)
     (void)]

    [else
     (define result
       (resolve (car var_env) var_name))

     (if (void? result)
         (resolve_env
          (cdr var_env)
          var_name)
         result)]))

; ==========================================
; VARIABLE INSERTION AND UPDATE
; ==========================================

(define (update_pair left_part lst key value)
  (cond
    [(null? lst)
     left_part]

    [(eq? (caar lst) key)
     (append
      left_part
      (list (list key value))
      (cdr lst))]

    [else
     (update_pair
      (append left_part (list (car lst)))
      (cdr lst)
      key
      value)]))

(define (update_scope var_scope
                      var_name
                      var_value)

  (unless (tagged_value? var_value)
    (error "INTERPRETER ERROR: untagged value."))

  (if (void? (resolve var_scope var_name))

      (cons
       (list var_name var_value)
       var_scope)

      (update_pair
       '()
       var_scope
       var_name
       var_value)))

; Original single-scope helpers
(define (dog var_scope var_name var_value)
  (unless (tagged_value? var_value)
    (error "INTERPRETER ERROR: untagged value."))

  (if (void? (resolve var_scope var_name))
      (set! scope
            (cons
             (list var_name var_value)
             var_scope))
      (error "Variable name has been used.")))

(define (wolf var_scope var_name var_value)
  (set! scope
        (update_scope
         var_scope
         var_name
         var_value)))

; Update the nearest existing variable.
; Otherwise insert into the top scope.

(define (wolf_env var_env var_name var_value)

  (unless (tagged_value? var_value)
    (error "INTERPRETER ERROR: untagged value."))

  (define (update-existing env)
    (cond
      [(null? env)
       '()]

      [(not
        (void?
         (resolve (car env) var_name)))

       (cons
        (update_scope
         (car env)
         var_name
         var_value)
        (cdr env))]

      [else
       (cons
        (car env)
        (update-existing (cdr env)))]))

  (cond
    [(null? var_env)
     (set! environment
           (list
            (list
             (list var_name var_value))))]

    [(void? (resolve_env var_env var_name))
     (set! environment
           (cons
            (cons
             (list var_name var_value)
             (car var_env))
            (cdr var_env)))]

    [else
     (set! environment
           (update-existing var_env))])

  var_value)

; ==========================================
; MATH OPERATORS
; ==========================================

(define (is_valid_math_op op)
  (if (memq op '(+ - * / // %))
      #t
      #f))

(define (do_math op left_operand right_operand)

  ; Unwrap the operands
  (define left_val
    (check_type 'num left_operand))

  (define right_val
    (check_type 'num right_operand))

  ; Perform the calculation and wrap the result
  (make_num
   (case op
     [(+) (+ left_val right_val)]
     [(-) (- left_val right_val)]
     [(*) (* left_val right_val)]
     [(/) (/ left_val right_val)]
     [(//) (quotient left_val right_val)]
     [(%) (modulo left_val right_val)]

     [else
      (error "Invalid math operator.")])))

; ==========================================
; BOOLEAN OPERATORS
; ==========================================

(define (is_valid_boolean_op op)
  (if (memq op '(> >= < <= == === && ||))
      #t
      #f))

(define (calculate_boolean op
                           left_operand
                           right_operand)

  (unless (and (tagged_value? left_operand)
               (tagged_value? right_operand))
    (error "INTERPRETER ERROR: untagged operand."))

  (make_bool
   (case op

     [(>)
      (> (check_type 'num left_operand)
         (check_type 'num right_operand))]

     [(>=)
      (>= (check_type 'num left_operand)
          (check_type 'num right_operand))]

     [(<)
      (< (check_type 'num left_operand)
         (check_type 'num right_operand))]

     [(<=)
      (<= (check_type 'num left_operand)
          (check_type 'num right_operand))]

     ; Compare values of the same type
     [(==)
      (unless
          (eq? (car left_operand)
               (car right_operand))
        (error
         "Data type error: operands must have the same type."))

      (equal?
       (cadr left_operand)
       (cadr right_operand))]

     ; Compare both the type and the value
     [(===)
      (equal? left_operand right_operand)]

     [(&&)
      (and
       (check_type 'bool left_operand)
       (check_type 'bool right_operand))]

     [(||)
      (or
       (check_type 'bool left_operand)
       (check_type 'bool right_operand))]

     [else
      (error "Invalid boolean operator.")])))

; ==========================================
; FUNCTION PARAMETER PAIRS
; ==========================================

(define (create_pair_list lst_ret lst1 lst2)
  (if (or (null? lst1) (null? lst2))

      (reverse lst_ret)

      (create_pair_list
       (cons
        (list (car lst1) (car lst2))
        lst_ret)
       (cdr lst1)
       (cdr lst2))))

; ==========================================
; ENVIRONMENT MANAGEMENT
; ==========================================

(define (update_base_environment new_env)
  (set! environment new_env))

(define (push_scope_to_environment new_scope)
  (set! environment
        (cons new_scope environment)))

(define (pop_scope_from_environment)
  (when (null? environment)
    (error "INTERPRETER ERROR: empty environment."))

  (set! environment
        (cdr environment)))

(provide (all-defined-out))
