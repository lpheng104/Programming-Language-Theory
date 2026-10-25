#lang racket

(require "util.rkt")

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
  (dynamic-wind
   (lambda ()
     (push_scope new-scope))
   thunk
   (lambda ()
     (pop_scope))))

; ==========================================
; HELPER: WHILE LOOP
; ==========================================

(define (execute-while condition body)
  (with-new-scope
   (lambda ()
     (let loop ((last-result (void)))
       (if (require-boolean (process condition))
           (let ((result (bulk-value (process body))))
             (loop result))
           last-result)))))

; ==========================================
; HELPER: DO-WHILE LOOP
; ==========================================

(define (execute-do-while body condition)
  (with-new-scope
   (lambda ()
     (let loop ()
       (let ((result (bulk-value (process body))))
         (if (require-boolean (process condition))
             (loop)
             result))))))

; ==========================================
; HELPER: FOR LOOP
; ==========================================

(define (execute-for initialization condition increment body)
  (with-new-scope
   (lambda ()

     ; Execute initialization once.
     (process initialization)

     ; Repeat while condition is true.
     (let loop ((last-result (void)))

       (if (require-boolean (process condition))

           (let ((result (bulk-value (process body))))

             ; Increment after executing body.
             (process increment)

             ; Repeat.
             (loop result))

           ; Condition false: stop.
           last-result)))))

; ==========================================
; HELPER: FUNCTION
; ==========================================

(define (execute-function parsed-exp)
  (let* ((pairs (cadr parsed-exp))
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

; ==========================================
; HELPER: BULK EXPRESSION
; ==========================================

(define (execute-bulk statements)
  (let loop ((remaining statements)
             (last-result (void)))

    (if (null? remaining)

        (list
         'terminator-exp
         (list 'bulk-ret last-result))

        (loop
         (cdr remaining)
         (process (car remaining))))))

; ==========================================
; MAIN INTERPRETER
; ==========================================

(define (process parsed-exp)
  (cond

    ; EMPTY PROGRAM
    [(void? parsed-exp)
     (void)]

    [(null? parsed-exp)
     (error "ERROR: EMPTY PROGRAM")]

    ; ======================================
    ; VARIABLE
    ; ======================================

    [(eq? 'var-exp (car parsed-exp))
     (let ((value
            (resolve_env environment
                         (cadr parsed-exp))))
       (if (void? value)
           (error "ERROR: variable not found"
                  (cadr parsed-exp))
           value))]

    ; NUMBER
    [(eq? 'num-exp (car parsed-exp))
     (cadr parsed-exp)]

    ; STRING
    [(eq? 'string-exp (car parsed-exp))
     (cadr parsed-exp)]

    ; ======================================
    ; MATH
    ; ======================================

    [(eq? 'math-exp (car parsed-exp))
     (let ((left (process (caddr parsed-exp)))
           (right (process (cadddr parsed-exp))))

       (if (and (number? left)
                (number? right))

           (do_math
            (cadr parsed-exp)
            left
            right)

           (error "ERROR: math operands must be numbers")))]

    ; ======================================
    ; BOOLEAN
    ; ======================================

    [(eq? 'boolean-exp (car parsed-exp))
     (let ((op (cadr parsed-exp)))

       (cond

         [(eq? op 'True)
          #t]

         [(eq? op 'False)
          #f]

         [(eq? op '!)
          (not
           (require-boolean
            (process (caddr parsed-exp))))]

         [(or (eq? op '&&)
              (eq? op 'and))
          (let ((left
                 (require-boolean
                  (process (caddr parsed-exp)))))
            (and left
                 (require-boolean
                  (process (cadddr parsed-exp)))))]

         [(or (eq? op '||)
              (eq? op 'or))
          (let ((left
                 (require-boolean
                  (process (caddr parsed-exp)))))
            (or left
                (require-boolean
                 (process (cadddr parsed-exp)))))]

         [else
          (calculate_boolean
           op
           (process (caddr parsed-exp))
           (process (cadddr parsed-exp)))]))]

    ; ======================================
    ; TASK 1: VARIABLE ASSIGNMENT
    ;
    ; (assign-exp (var-exp a) (num-exp 1))
    ; ======================================

    [(eq? 'assign-exp (car parsed-exp))
     (let ((variable (cadr (cadr parsed-exp)))
           (value (process (caddr parsed-exp))))

       ; Update existing variable or insert new.
       (wolf_env environment variable value)

       ; Return assigned value.
       value)]

    ; ======================================
    ; ASK / IF CONDITION
    ; ======================================

    [(eq? 'ask-exp (car parsed-exp))
     (if (require-boolean
          (process (cadr parsed-exp)))

         (process (caddr parsed-exp))

         (process (cadddr parsed-exp)))]

    ; ======================================
    ; WHILE LOOP
    ; ======================================

    [(eq? 'while-exp (car parsed-exp))
     (execute-while
      (cadr parsed-exp)
      (caddr parsed-exp))]

    ; ======================================
    ; DO-WHILE LOOP
    ; ======================================

    [(eq? 'do-while-exp (car parsed-exp))
     (execute-do-while
      (cadr parsed-exp)
      (caddr parsed-exp))]

    ; ======================================
    ; TASK 2: FOR LOOP
    ; ======================================

    [(eq? 'for-exp (car parsed-exp))
     (execute-for
      (cadr parsed-exp)
      (caddr parsed-exp)
      (cadddr parsed-exp)
      (list-ref parsed-exp 4))]

    ; ======================================
    ; FUNCTION
    ; ======================================

    [(eq? 'func-exp (car parsed-exp))
     (execute-function parsed-exp)]

    ; ======================================
    ; BULK EXPRESSION
    ; ======================================

    [(eq? 'bulk-exp (car parsed-exp))
     (execute-bulk (cdr parsed-exp))]

    ; ======================================
    ; UNSUPPORTED
    ; ======================================

    [else
     (error "ERROR: unsupported expression")]))

(provide (all-defined-out))
