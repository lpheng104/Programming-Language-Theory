#lang racket

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(process (parse '(
  main (
    (a = 1)
    (b = 2)

    (func add (a b) (
      (c = (a + b))
      (return c)
    ))

    (c = (call add (a b)))
    (fire c)
  )
)))

; ==========================================
; PART 1B: GCD USING A WHILE LOOP
; ==========================================

(process (parse '(
  main (

    (func gcd_loop (x y) (
      (while (y > 0) (
        (temp = y)
        (y = (x % y))
        (x = temp)
      ))
      (return x)
    ))

    ; Test 1: Expected 6
    (fire (call gcd_loop (48 18)))

    ; Test 2: Expected 1
    (fire (call gcd_loop (17 5)))

    ; Test 3: Expected 25
    (fire (call gcd_loop (100 75)))

  )
)))

; ==========================================
; PART 1B: GCD USING RECURSION
; ==========================================

(process (parse '(
  main (

    (func gcd_recursive (x y) (
      (ask (y == 0)
        (
          (return x)
        )
        (
          (return
            (call gcd_recursive (y (x % y))))
        )
      )
    ))

    ; Test 4: Expected 6
    (fire (call gcd_recursive (48 18)))

    ; Test 5: Expected 1
    (fire (call gcd_recursive (17 5)))

    ; Test 6: Expected 25
    (fire (call gcd_recursive (100 75)))

  )
)))
