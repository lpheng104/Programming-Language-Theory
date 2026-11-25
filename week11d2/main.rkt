#lang racket

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

; ==========================================
; WEEK 11 DAY 2 HOMEWORK
; TAGGED DATA TYPES
; ==========================================

; ==========================================
; SECTION 1: VARIABLES AND LITERALS
; ==========================================

(process (parse '(
  main

  (fire a)        ;1
  (fire "hello")  ;hello
  (fire True)     ;True
)))

; ==========================================
; SECTION 2: MATH AND REUSING RESULTS
; ==========================================

(process (parse '(
  main

  (fire (a + b))          ;3
  (fire ((a + b) * 2))    ;6

  (n = ((a + b) * 2))

  (fire (n - 1))          ;5
)))

; ==========================================
; SECTION 3: COMPARISON AND LOGIC
; ==========================================

(process (parse '(
  main

  (fire (a < b))          ;True

  (g = (a < b))

  (fire (g && e))         ;True
  (fire (! e))            ;False
)))

; ==========================================
; SECTION 4: STRINGS AND EQUALITY
; ==========================================

(process (parse '(
  main

  (s = "abc")

  (fire s)                ;abc
  (fire (s == "abc"))      ;True
  (fire (a === c))        ;False
)))

; ==========================================
; SECTION 5: BOOLEAN CONDITIONS
; ==========================================

(process (parse '(
  main

  (ask f
    ((fire "wrong"))
    ((fire "right"))
  )                       ;right

  (running = True)
  (k = 0)

  (while running (
    (k = (k + 1))

    (ask (k == 3)
      ((running = False))
      ()
    )
  ))

  (fire k)                ;3

  (more = False)
  (m = 0)

  (do-while
    ((m = (m + 1)))
    more
  )

  (fire m)                ;1
)))

; ==========================================
; SECTION 6: FUNCTIONS
; ==========================================

(process (parse '(
  main

  (func add (x y) (
    (z = (x + y))
    (return z)
  ))

  (fire (call add (3 4)))  ;7

  (func is_big (x) (
    (ask (x > 10)
      ((return True))
      ((return False))
    )
  ))

  (fire (call is_big (20))) ;True

  (func nothing () (
    (z = 1)
  ))

  (fire (call nothing ())) ;void
)))

; ==========================================
; SECTION 7: GCD
; ==========================================

; Loop version of GCD

(process (parse '(
  main

  (func gcd_loop (x y) (

    (while (y > 0) (
      (temp = y)
      (y = (x % y))
      (x = temp)
    ))

    (return x)
  ))
)))

; Recursive version of GCD

(process (parse '(
  main

  (func gcd_rec (x y) (

    (ask (y == 0)
      (
        (return x)
      )
      (
        (return
          (call gcd_rec (y (x % y))))
      )
    )

  ))
)))

; Loop GCD tests

(process
 (parse '(fire (call gcd_loop (48 18)))))  ;6

(process
 (parse '(fire (call gcd_loop (17 5)))))   ;1

(process
 (parse '(fire (call gcd_loop (100 75))))) ;25

; Recursive GCD tests

(process
 (parse '(fire (call gcd_rec (48 18)))))   ;6

(process
 (parse '(fire (call gcd_rec (17 5)))))    ;1

(process
 (parse '(fire (call gcd_rec (100 75)))))  ;25

; ==========================================
; SECTION 8: PRINT ENVIRONMENT
; ==========================================

; Every value stored in the environment
; should have a type tag.

(displayln environment)
