#lang racket

(require "assignment3.rkt")

;; Tests for part A
(display "test1\n")

(define example3 "-(2+3)*4/5")

(equal? (ast<-string example3)
        (quo (prod (neg (sum 2 3)) 4) 5))

(display "test2\n")

(define example4 "let x = 2+3, y = x*4 in 10*y+y")

(equal? (meaning (ast<-string example4) empty-env init-k empty-store) 220)

(display "test3\n")

(define example5 "let x = 0 in {x = x + 2; x = x + 3; x = x*4; x}")

(equal? (meaning (ast<-string example5) empty-env init-k empty-store) 20)

(display "test4\n")

(define example6 "let d = \\a,b,c\\->b*b-4*a*c in d(1, -3, 2)")

(equal? (meaning (ast<-string example6) empty-env init-k empty-store) 1)
