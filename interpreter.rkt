#lang racket

(require "util.rkt")

; ==========================================
; RETURN VALUE HELPER
; ==========================================

(define (return-value? value)
  (and (list? value)
       (= (length value) 2)
       (eq? (car value) 'return-val)))

; ==========================================
; BOOLEAN HELPER
; ==========================================

(define (require-boolean value)
  (unless (boolean? value)
    (error
     "INTERPRETER ERROR: condition must be boolean."))
  value)

; ==========================================
; SCOPE HELPER
; ==========================================

(define (with-new-scope new-scope thunk)
  (dynamic-wind
   (lambda ()
     (push_scope new-scope))
   thunk
   (lambda ()
     (pop_scope))))

; ==========================================
; PART 1: EXECUTE STATEMENT LIST
;
; Run statements in order.
; Stop immediately on return-val.
; Otherwise return void.
; ==========================================

(define (execute-list statements)
  (cond
    [(null? statements)
     (void)]

    [else
     (define result
       (process (car statements)))

     (if (return-value? result)
         result
         (execute-list (cdr statements)))]))

; ==========================================
; PART 2: DEFINE NAMED FUNCTION
; ==========================================

(define (execute-function-definition parsed-exp)

  (define function-name
    (cadr (cadr parsed-exp)))

  (define parameters
    (map cadr (caddr parsed-exp)))

  (define body
    (cadddr parsed-exp))

  ; A function is stored as a value.
  (define function-value
    (list 'func-val parameters body))

  (wolf_env environment
            function-name
            function-value)

  (void))

; ==========================================
; PART 2: CALL NAMED FUNCTION
; ==========================================

(define (execute-function-call parsed-exp)

  ; Step 1: Find function by name.
  (define function-name
    (cadr (cadr parsed-exp)))

  (define function-value
    (resolve_env environment function-name))

  ; Step 2: Confirm that it is a function.
  (unless
      (and (list? function-value)
           (= (length function-value) 3)
           (eq? (car function-value) 'func-val))
    (error
     "INTERPRETER ERROR: not a function."))

  ; Step 3: Get parameters and body.
  (define parameters
    (cadr function-value))

  (define body
    (caddr function-value))

  ; Step 4: Evaluate arguments in caller's scope.
  (define arguments
    (map process (caddr parsed-exp)))

  ; Step 5: Check number of arguments.
  (unless (= (length parameters)
             (length arguments))
    (error
     "INTERPRETER ERROR: wrong number of arguments."))

  ; Step 6: Bind parameters to argument values.
  (define new-scope
    (map list parameters arguments))

  ; Step 7: Execute function in new scope.
  (define result
    (with-new-scope
     new-scope
     (lambda ()
       (process body))))

  ; Step 8: Unwrap return value.
  (if (return-value? result)
      (cadr result)
      (void)))

; ==========================================
; WHILE LOOP
; ==========================================

(define (execute-while condition body)

  (with-new-scope
   '()
   (lambda ()

     (let loop ()

       (if (require-boolean (process condition))

           (let ((result (process body)))

             ; Return immediately if body returns.
             (if (return-value? result)
                 result
                 (loop)))

           (void))))))

; ==========================================
; DO-WHILE LOOP
; ==========================================

(define (execute-do-while body condition)

  (with-new-scope
   '()
   (lambda ()

     (let loop ()

       (define result
         (process body))

       (cond
         [(return-value? result)
          result]

         [(require-boolean (process condition))
          (loop)]

         [else
          (void)])))))

; ==========================================
; FOR LOOP
; ==========================================

(define (execute-for initialization
                     condition
                     increment
                     body)

  (with-new-scope
   '()
   (lambda ()

     ; Initialization executes once.
     (define initial-result
       (process initialization))

     (if (return-value? initial-result)
         initial-result

         (let loop ()

           (if (require-boolean (process condition))

               (let ((result (process body)))

                 (if (return-value? result)
                     result

                     (let ((increment-result
                            (process increment)))

                       (if (return-value? increment-result)
                           increment-result
                           (loop)))))

               (void)))))))

; ==========================================
; OLD ANONYMOUS FUNCTION SUPPORT
; ==========================================

(define (execute-anonymous-function parsed-exp)

  (define parameters
    (map cadr (cadr parsed-exp)))

  (define arguments
    (map process (caddr parsed-exp)))

  (define body
    (cadddr parsed-exp))

  (unless (= (length parameters)
             (length arguments))
    (error
     "INTERPRETER ERROR: wrong number of arguments."))

  (define result
    (with-new-scope
     (map list parameters arguments)
     (lambda ()
       (process body))))

  (if (return-value? result)
      (cadr result)
      (void)))

; ==========================================
; MAIN INTERPRETER
; ==========================================

(define (process parsed-exp)

  (cond

    ; ======================================
    ; EMPTY
    ; ======================================

    [(void? parsed-exp)
     (void)]

    [(null? parsed-exp)
     (void)]

    ; ======================================
    ; VARIABLE
    ; ======================================

    [(eq? (car parsed-exp) 'var-exp)
     (resolve_env environment
                  (cadr parsed-exp))]

    ; ======================================
    ; NUMBER
    ; ======================================

    [(eq? (car parsed-exp) 'num-exp)
     (cadr parsed-exp)]

    ; ======================================
    ; STRING
    ; ======================================

    [(eq? (car parsed-exp) 'string-exp)
     (cadr parsed-exp)]

    ; ======================================
    ; MATH
    ;
    ; Evaluate each operand exactly once.
    ; ======================================

    [(eq? (car parsed-exp) 'math-exp)

     (define operator
       (cadr parsed-exp))

     (define left
       (process (caddr parsed-exp)))

     (define right
       (process (cadddr parsed-exp)))

     (unless (and (number? left)
                  (number? right))
       (error
        "INTERPRETER ERROR: math operands must be numbers."))

     (do_math operator left right)]

    ; ======================================
    ; BOOLEAN
    ; ======================================

    [(eq? (car parsed-exp) 'boolean-exp)

     (define operator
       (cadr parsed-exp))

     (cond

       [(eq? operator 'True)
        #t]

       [(eq? operator 'False)
        #f]

       [(eq? operator '!)
        (not
         (require-boolean
          (process (caddr parsed-exp))))]

       [(or (eq? operator '&&)
            (eq? operator 'and))

        (and
         (require-boolean
          (process (caddr parsed-exp)))
         (require-boolean
          (process (cadddr parsed-exp))))]

       [(or (eq? operator '||)
            (eq? operator 'or))

        (or
         (require-boolean
          (process (caddr parsed-exp)))
         (require-boolean
          (process (cadddr parsed-exp))))]

       [else
        (define left
          (process (caddr parsed-exp)))

        (define right
          (process (cadddr parsed-exp)))

        (calculate_boolean
         operator left right)])]

    ; ======================================
    ; TASK 1: VARIABLE ASSIGNMENT
    ; ======================================

    [(eq? (car parsed-exp) 'assign-exp)

     (define variable-name
       (cadr (cadr parsed-exp)))

     (define value
       (process (caddr parsed-exp)))

     (wolf_env environment variable-name value)

     value]

    ; ======================================
    ; PART 1: STATEMENT LIST
    ; ======================================

    [(eq? (car parsed-exp) 'list-exp)
     (execute-list (cdr parsed-exp))]

    ; ======================================
    ; PART 2: FUNCTION DEFINITION
    ; ======================================

    [(eq? (car parsed-exp) 'func-def-exp)
     (execute-function-definition parsed-exp)]

    ; ======================================
    ; PART 2: FUNCTION CALL
    ; ======================================

    [(eq? (car parsed-exp) 'call-exp)
     (execute-function-call parsed-exp)]

    ; ======================================
    ; PART 2: RETURN
    ; ======================================

    [(eq? (car parsed-exp) 'return-exp)
     (list
      'return-val
      (process (cadr parsed-exp)))]

    ; ======================================
    ; FIRE / PRINT
    ; ======================================

    [(eq? (car parsed-exp) 'fire-exp)

     (define value
       (process (cadr parsed-exp)))

     (write value)
     (newline)

     (void)]

    ; ======================================
    ; ASK / IF
    ; ======================================

    [(eq? (car parsed-exp) 'ask-exp)

     (if (require-boolean
          (process (cadr parsed-exp)))

         (process (caddr parsed-exp))

         (process (cadddr parsed-exp)))]

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
    ; OLD ANONYMOUS FUNCTION
    ; ======================================

    [(eq? (car parsed-exp) 'func-exp)
     (execute-anonymous-function parsed-exp)]

    ; ======================================
    ; UNSUPPORTED
    ; ======================================

    [else
     (error
      "INTERPRETER ERROR: unsupported expression.")]))

(provide (all-defined-out))
