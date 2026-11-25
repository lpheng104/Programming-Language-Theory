#lang racket

(require "util.rkt")

; ==========================================
; RETURN SLOT
; ==========================================

(define no_return
  (list 'none #f))

(define (has_returned)
  (define ret
    (resolve_env environment '|#return|))

  (and
   (not (void? ret))
   (not (equal? ret no_return))))

; ==========================================
; SCOPE MANAGEMENT
; ==========================================

(define (with-scope new_scope thunk)
  (dynamic-wind
   (lambda ()
     (push_scope_to_environment new_scope))
   thunk
   (lambda ()
     (pop_scope_from_environment))))

; ==========================================
; EXECUTE STATEMENTS
; ==========================================

(define (execute-list statements)
  (cond
    [(null? statements)
     (make_void)]

    [(has_returned)
     (make_void)]

    [else
     (process (car statements))

     (if (has_returned)
         (make_void)
         (execute-list (cdr statements)))]))

; ==========================================
; EXECUTE FUNCTION
; ==========================================

(define (execute-function parameters
                          arguments
                          body)

  (unless
      (= (length parameters)
         (length arguments))
    (error
     "INTERPRETER ERROR: wrong number of arguments."))

  ; Arguments remain tagged when stored
  (define new_scope
    (map list parameters arguments))

  ; Every function receives a return slot
  (with-scope
   (cons
    (list '|#return| no_return)
    new_scope)

   (lambda ()

     (process body)

     ; Read the return value before popping scope
     (let ((ret
            (resolve_env
             environment
             '|#return|)))

       (if (equal? ret no_return)
           (make_void)
           ret)))))

; ==========================================
; NAMED FUNCTION CALL
; ==========================================

(define (execute-call parsed-exp)

  (define function_name
    (cadr (cadr parsed-exp)))

  (define function_value
    (resolve_env
     environment
     function_name))

  (unless
      (and (tagged_value? function_value)
           (eq? (car function_value) 'func))
    (error
     "INTERPRETER ERROR: not a function."))

  ; Unwrap the function definition
  (define definition
    (check_type 'func function_value))

  (define parameters
    (car definition))

  (define body
    (cadr definition))

  ; Evaluate arguments in the caller's scope
  (define arguments
    (map process (caddr parsed-exp)))

  (execute-function
   parameters
   arguments
   body))

; ==========================================
; WHILE LOOP
; ==========================================

(define (execute-while condition body)

  (let loop ()

    (cond
      [(has_returned)
       (make_void)]

      ; Unwrap the boolean condition
      [(check_type 'bool (process condition))

       (with-scope
        '()
        (lambda ()
          (process body)))

       (if (has_returned)
           (make_void)
           (loop))]

      [else
       (make_void)])))

; ==========================================
; DO-WHILE LOOP
; ==========================================

(define (execute-do-while body condition)

  (let loop ()

    ; Run body at least once
    (with-scope
     '()
     (lambda ()
       (process body)))

    (cond
      [(has_returned)
       (make_void)]

      [(check_type 'bool (process condition))
       (loop)]

      [else
       (make_void)])))

; ==========================================
; FOR LOOP
; ==========================================

(define (execute-for initialization
                     condition
                     increment
                     body)

  (with-scope
   '()

   (lambda ()

     (process initialization)

     (let loop ()

       (cond
         [(has_returned)
          (make_void)]

         [(check_type 'bool (process condition))

          (with-scope
           '()
           (lambda ()
             (process body)))

          (unless (has_returned)
            (process increment))

          (if (has_returned)
              (make_void)
              (loop))]

         [else
          (make_void)])))))

; ==========================================
; FIRE / PRINT
; ==========================================

(define (print-value value)

  (unless (tagged_value? value)
    (error "INTERPRETER ERROR: untagged value."))

  (case (car value)

    [(bool)
     (displayln
      (if (check_type 'bool value)
          "True"
          "False"))]

    [(void)
     (displayln "void")]

    [else
     (displayln (cadr value))])

  (make_void))

; ==========================================
; MAIN INTERPRETER
; ==========================================

(define (process parsed-exp)

  (cond

    ; Empty expressions
    [(or (void? parsed-exp)
         (null? parsed-exp))
     (make_void)]

    ; ======================================
    ; VARIABLE
    ; ======================================

    [(eq? (car parsed-exp) 'var-exp)

     (define result
       (resolve_env
        environment
        (cadr parsed-exp)))

     (if (void? result)
         (make_void)
         result)]

    ; ======================================
    ; NUMBER AND STRING
    ; ======================================

    [(eq? (car parsed-exp) 'num-exp)
     (cadr parsed-exp)]

    [(eq? (car parsed-exp) 'string-exp)
     (cadr parsed-exp)]

    ; ======================================
    ; ASSIGNMENT
    ; ======================================

    [(eq? (car parsed-exp) 'assign-exp)

     (define variable_name
       (cadr (car (cadr parsed-exp))))

     (define variable_value
       (process (cadr (cadr parsed-exp))))

     (wolf_env
      environment
      variable_name
      variable_value)

     variable_value]

    ; ======================================
    ; MATH
    ; ======================================

    [(eq? (car parsed-exp) 'math-exp)

     (define left_operand
       (process (caddr parsed-exp)))

     (define right_operand
       (process (cadddr parsed-exp)))

     (do_math
      (cadr parsed-exp)
      left_operand
      right_operand)]

    ; ======================================
    ; BOOLEAN EXPRESSIONS
    ; ======================================

    [(eq? (car parsed-exp) 'boolean-exp)

     (define operator
       (cadr parsed-exp))

     (cond

       [(eq? operator 'True)
        (make_bool #t)]

       [(eq? operator 'False)
        (make_bool #f)]

       [(eq? operator '!)
        (make_bool
         (not
          (check_type
           'bool
           (process (caddr parsed-exp)))))]

       [else
        (calculate_boolean
         operator
         (process (caddr parsed-exp))
         (process (cadddr parsed-exp)))])]

    ; ======================================
    ; FUNCTION DEFINITION
    ; ======================================

    [(eq? (car parsed-exp) 'new-func-exp)

     ; Store the definition as a tagged function
     (list
      'func
      (list
       (map cadr (cadr parsed-exp))
       (caddr parsed-exp)))]

    ; ======================================
    ; NAMED FUNCTION CALL
    ; ======================================

    [(eq? (car parsed-exp) 'call-exp)
     (execute-call parsed-exp)]

    ; ======================================
    ; ANONYMOUS FUNCTION
    ; ======================================

    [(eq? (car parsed-exp) 'func-exp)

     (define pairs
       (cadr parsed-exp))

     (define parameters
       (map
        (lambda (pair)
          (cadr (car pair)))
        pairs))

     (define arguments
       (map
        (lambda (pair)
          (process (cadr pair)))
        pairs))

     (execute-function
      parameters
      arguments
      (caddr parsed-exp))]

    ; ======================================
    ; RETURN
    ; ======================================

    [(eq? (car parsed-exp) 'return-exp)

     (when
         (void?
          (resolve_env environment '|#return|))
       (error
        "INTERPRETER ERROR: return outside function."))

     (define value
       (process (cadr parsed-exp)))

     (wolf_env
      environment
      '|#return|
      value)

     value]

    ; ======================================
    ; ASK / IF
    ; ======================================

    [(eq? (car parsed-exp) 'ask-exp)

     (if
      (check_type
       'bool
       (process (cadr parsed-exp)))

      (process (caddr parsed-exp))

      (process (cadddr parsed-exp)))]

    ; ======================================
    ; STATEMENT LIST
    ; ======================================

    [(eq? (car parsed-exp) 'list-exp)
     (execute-list (cdr parsed-exp))]

    ; ======================================
    ; BULK EXPRESSIONS
    ; ======================================

    [(eq? (car parsed-exp) 'bulk-exp)

     (let loop ((statements (cdr parsed-exp))
                (result (make_void)))

       (if (or (null? statements)
               (has_returned))

           result

           (loop
            (cdr statements)
            (process (car statements)))))]

    ; ======================================
    ; WHILE
    ; ======================================

    [(eq? (car parsed-exp) 'while-exp)
     (execute-while
      (cadr parsed-exp)
      (caddr parsed-exp))]

    ; ======================================
    ; DO-WHILE
    ; ======================================

    [(eq? (car parsed-exp) 'do-while-exp)
     (execute-do-while
      (cadr parsed-exp)
      (caddr parsed-exp))]

    ; ======================================
    ; FOR
    ; ======================================

    [(eq? (car parsed-exp) 'for-exp)
     (execute-for
      (cadr parsed-exp)
      (caddr parsed-exp)
      (cadddr parsed-exp)
      (list-ref parsed-exp 4))]

    ; ======================================
    ; FIRE
    ; ======================================

    [(eq? (car parsed-exp) 'fire-exp)
     (print-value
      (process (cadr parsed-exp)))]

    ; ======================================
    ; UNSUPPORTED EXPRESSION
    ; ======================================

    [else
     (error
      "INTERPRETER ERROR: unsupported expression.")]))

(provide (all-defined-out))

