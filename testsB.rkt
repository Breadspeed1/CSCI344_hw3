#lang racket

;; Tests for part B

(equal? (ast<-string example3)
        (quo (prod (neg (sum 2 3)) 4) 5))

(define example4 "let x = 2+3, y = x*4 in 10*y+y")

(equal? (meaning (ast<-string example4) empty-env init-k) 220)

(define example5 "let d = \\a,b,c\\->b*b-4*a*c in d(1, -3, 2)")

(equal? (meaning (ast<-string example5) empty-env init-k) 1)

(define example6 "let x = 1/0 in 3")

(equal? (meaning (ast<-string example6) empty-env init-k) 3)

(define example7 "let x = z in 3")

(equal? (meaning (ast<-string example7) empty-env init-k) 3)

(define example8 "(\\x,y\\->x)(1,1/0)")

(equal? (meaning (ast<-string example8) empty-env init-k) 1)

(define example9 "(\\x,y\\->x)(1,z)")

(equal? (meaning (ast<-string example9) empty-env init-k) 1)

