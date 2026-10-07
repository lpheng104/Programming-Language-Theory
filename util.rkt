#lang racket

; ==========================================
; ENVIRONMENT
; ==========================================

(define
  scope
  (list
   (list 'a 1)
   (list 'b 2)
   )
  )

; var_env := var_scope x (var_env)
(define
  environment
  (list scope)
  )


; ==========================================
; VARIABLE LOOKUP
; ==========================================

; Search for a variable inside one scope
(define
  resolve
  (lambda (var_scope var_name)
    (cond
      ((null? var_scope)
       (void))

      ((eq? (car (car var_scope)) var_name)
       (cadr (car var_scope)))

      (else
       (resolve (cdr var_scope) var_name))
      )
    )
  )


; Search for a variable through the environment
(define
  resolve_env
  (lambda (var_env var_name)
    (cond
      ((null? var_env)
       (void))

      ((void? (resolve (car var_env) var_name))
       (resolve_env (cdr var_env) var_name))

      (else
       (resolve (car var_env) var_name))
      )
    )
  )


; ==========================================
; VARIABLE INSERT
; ==========================================

(define
  dog
  (lambda (var_scope var_name var_value)
    (cond
      ((null? var_scope)
       (set! scope
             (cons (list var_name var_value)
                   var_scope)))

      ((void? (resolve var_scope var_name))
       (set! scope
             (cons (list var_name var_value)
                   var_scope)))

      (else
       (displayln
        "### ERROR ### Variable name has been used."))
      )
    )
  )


; ==========================================
; UPDATE VARIABLE
; ==========================================

(define
  update_pair
  (lambda (left_part lst key value)
    (cond
      ((null? lst)
       left_part)

      ((eq? (car (car lst)) key)
       (append
        left_part
        (list (list key value))
        (cdr lst)))

      (else
       (update_pair
        (append
         left_part
         (list (car lst)))
        (cdr lst)
        key
        value))
      )
    )
  )


; Insert or update
(define
  wolf
  (lambda (var_scope var_name var_value)
    (cond
      ((null? var_scope)
       (dog var_scope var_name var_value))

      ((void? (resolve var_scope var_name))
       (dog var_scope var_name var_value))

      (else
       (set! scope
             (update_pair
              '()
              var_scope
              var_name
              var_value)))
      )
    )
  )


; Return an updated scope
(define
  update_scope
  (lambda (var_scope var_name var_value)
    (cond
      ((null? var_scope)
       (list
        (list var_name var_value)))

      ((void? (resolve var_scope var_name))
       (cons
        (list var_name var_value)
        var_scope))

      (else
       (update_pair
        '()
        var_scope
        var_name
        var_value))
      )
    )
  )


; ==========================================
; UPDATE ENVIRONMENT
; ==========================================

(define
  wolf_env
  (lambda (var_env var_name var_value)
    (cond

      ; Variable does not exist yet
      ((void? (resolve_env var_env var_name))

       (if
        (null? var_env)

        (list
         (list var_name var_value))

        (set!
         environment
         (append
          (list
           (cons
            (list var_name var_value)
            (car var_env)))
          (cdr var_env)))
        )
       )

      ; Variable already exists
      (else

       (letrec
           (
            (update_env
             (lambda (left right)

               (cond
                 ((null? right)
                  left)

                 ((not
                   (void?
                    (resolve
                     (car right)
                     var_name)))

                  (append
                   left

                   (list
                    (update_scope
                     (car right)
                     var_name
                     var_value))

                   (cdr right)))

                 (else
                  (update_env
                   (append
                    left
                    (list (car right)))

                   (cdr right)))
                 )
               )
             )
            )

         (set!
          environment
          (update_env '() var_env))
         )
       )
      )
    )
  )


; ==========================================
; VALID MATH OPERATORS
; ==========================================

(define
  is_valid_math_op
  (lambda (op)
    (cond
      ((equal? '+ op) #t)
      ((equal? '- op) #t)
      ((equal? '* op) #t)
      ((equal? '/ op) #t)
      ((equal? '// op) #t)
      ((equal? '% op) #t)
      (else #f)
      )
    )
  )


; ==========================================
; DO MATH
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
      )
    )
  )


; ==========================================
; BOOLEAN OPERATORS
; NEW FOR THIS ASSIGNMENT
; ==========================================

(define
  is_valid_boolean_op
  (lambda (op)
    (cond
      ((eq? '> op) #t)
      ((eq? '== op) #t)
      ((eq? '< op) #t)
      ((eq? '>= op) #t)
      ((eq? '<= op) #t)
      ((eq? 'and op) #t)
      ((eq? 'or op) #t)
      (else #f)
      )
    )
  )


; ==========================================
; DO BOOLEAN OPERATION
; ==========================================

(define
  do_boolean
  (lambda (op left_operand right_operand)
    (cond

      ((eq? '> op)
       (> left_operand right_operand))

      ((eq? '< op)
       (< left_operand right_operand))

      ((eq? '>= op)
       (>= left_operand right_operand))

      ((eq? '<= op)
       (<= left_operand right_operand))

      ((eq? '== op)
       (equal? left_operand right_operand))

      ((eq? 'and op)
       (and left_operand right_operand))

      ((eq? 'or op)
       (or left_operand right_operand))

      (else
       (displayln
        "ERROR: unsupported boolean operator"))
      )
    )
  )


; ==========================================
; CREATE PARAMETER/VALUE PAIRS
; ==========================================

(define
  create_pair_list
  (lambda (lst_ret lst1 lst2)
    (if
     (or
      (null? lst1)
      (null? lst2))

     lst_ret

     (create_pair_list
      (cons
       (list
        (car lst1)
        (car lst2))
       lst_ret)

      (cdr lst1)
      (cdr lst2))
     )
    )
  )


; ==========================================
; UPDATE BASE ENVIRONMENT
; ==========================================

(define
  update_base_environment
  (lambda (new_env)
    (set! environment new_env)
    )
  )


(provide (all-defined-out))
