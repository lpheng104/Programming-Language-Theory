#lang racket
(require "util.rkt")

; ==========================================
; PROCESS PARSED EXPRESSIONS
; ==========================================

(define (process parsed-exp)

  (cond

    ((void? parsed-exp)
     (void))

    ((null? parsed-exp)
     (error "ERROR: EMPTY PROGRAM"))

    ; ======================================
    ; VARIABLE
    ; ======================================

    ((eq? 'var-exp (car parsed-exp))
     (let ((value
            (resolve_env environment
                         (cadr parsed-exp))))
       (if (void? value)
           (error "ERROR: variable not found"
                  (cadr parsed-exp))
           value)))

    ; ======================================
    ; NUMBER
    ; ======================================

    ((eq? 'num-exp (car parsed-exp))
     (cadr parsed-exp))

    ; ======================================
    ; STRING
    ; ======================================

    ((eq? 'string-exp (car parsed-exp))
     (cadr parsed-exp))

    ; ======================================
    ; MATH
    ; ======================================

    ((eq? 'math-exp (car parsed-exp))
     (let ((left (process (caddr parsed-exp)))
           (right (process (cadddr parsed-exp))))

       (unless (and (number? left)
                    (number? right))
         (error "ERROR: math operands must be numbers"))

       (do_math (cadr parsed-exp)
                left
                right)))

    ; ======================================
    ; BOOLEAN
    ; ======================================

    ((eq? 'boolean-exp (car parsed-exp))
     (let ((op (cadr parsed-exp)))

       (cond

         ((eq? op 'True) #t)

         ((eq? op 'False) #f)

         ((eq? op '!)
          (not (process (caddr parsed-exp))))

         ((or (eq? op '&&) (eq? op 'and))
          (let ((left (process (caddr parsed-exp))))
            (and (require-boolean left)
                 (require-boolean
                  (process (cadddr parsed-exp))))))

         ((or (eq? op '||) (eq? op 'or))
          (let ((left (process (caddr parsed-exp))))
            (or (require-boolean left)
                (require-boolean
                 (process (cadddr parsed-exp))))))

         (else
          (calculate_boolean
           op
           (process (caddr parsed-exp))
           (process (cadddr parsed-exp))))))

    ; ======================================
    ; SET / ASSIGNMENT
    ; ======================================

    ((eq? 'set-exp (car parsed-exp))
     (let ((variable (cadr parsed-exp))
           (value (process (caddr parsed-exp))))

       (wolf_env environment variable value)
       value))

    ; ======================================
    ; ASK / IF CONDITION
    ; ======================================

    ((eq? 'ask-exp (car parsed-exp))
     (if (require-boolean
          (process (cadr parsed-exp)))

         (process (caddr parsed-exp))
         (process (cadddr parsed-exp))))

    ; ======================================
    ; WHILE LOOP
    ; ======================================

    ((eq? 'while-exp (car parsed-exp))

     (let ((condition (cadr parsed-exp))
           (body (caddr parsed-exp)))

       ; Give the loop its own scope.
       (with-new-scope
        (lambda ()

          (let loop ((last-result (void)))

            (if (require-boolean
                 (process condition))

                (let ((result (process body)))

                  (loop
                   (bulk-value result)))

                last-result))))))

    ; ======================================
    ; DO-WHILE LOOP
    ; ======================================

    ((eq? 'do-while-exp (car parsed-exp))

     (let ((body (cadr parsed-exp))
           (condition (caddr parsed-exp)))

       (with-new-scope
        (lambda ()

          (let loop ()

            ; Run body first, no condition check.
            (let ((result
                   (bulk-value (process body))))

              ; Check condition after body.
              (if (require-boolean
                   (process condition))

                  (loop)

                  result)))))))

    ; ======================================
    ; FUNCTION
    ; ======================================

    ((eq? 'func-exp (car parsed-exp))

     (let* ((pairs (cadr parsed-exp))

            ; Evaluate arguments before adding
            ; the function's new scope.
            (new-scope
             (map
              (lambda (pair)
                (list
                 (cadr (car pair))
                 (process (cadr pair))))
              pairs))

            (body (caddr parsed-exp)))

       (with-new-scope
        (lambda ()
          (bulk-value (process body)))
        new-scope)))

    ; ======================================
    ; BULK EXPRESSION
    ; ======================================

    ((eq? 'bulk-exp (car parsed-exp))

     (let loop ((statements (cdr parsed-exp))
                (last-result (void)))

       (if (null? statements)

           (list
            'terminator-exp
            (list 'bulk-ret last-result))

           (loop
            (cdr statements)
            (process (car statements))))))

    ; ======================================
    ; UNSUPPORTED
    ; ======================================

    (else
     (error "ERROR: unsupported expression"))))

; ==========================================
; HELPER: REQUIRE BOOLEAN
; ==========================================

(define (require-boolean value)
  (unless (boolean? value)
    (error "ERROR: condition must be boolean"))
  value)

; ==========================================
; HELPER: GET BULK RETURN VALUE
; ==========================================

(define (bulk-value result)
  (if (and (list? result)
           (= (length result) 2)
           (eq? (car result) 'terminator-exp))
      (cadr (cadr result))
      result))

; ==========================================
; HELPER: TEMPORARY SCOPE
; ==========================================

(define (with-new-scope thunk [new-scope '()])
  (push_scope new-scope)

  (dynamic-wind
   void
   thunk
   (lambda ()
     (pop_scope))))

(provide (all-defined-out))
