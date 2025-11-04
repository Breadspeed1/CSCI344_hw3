#lang racket

(require "assignment3.rkt")

;; Extra Credit Tests

(equal? (ast<-string-extra-credit  example3)
        (quo (prod (neg (sum 2 3)) 4) 5))

(equal? (meaning-extra-credit (ast<-string-extra-credit  example4) empty-env init-k) 220)

(equal? (meaning-extra-credit (ast<-string-extra-credit  example5) empty-env init-k) 1)

(equal? (meaning-extra-credit (ast<-string-extra-credit  example6) empty-env init-k) 3)

(equal? (meaning-extra-credit (ast<-string-extra-credit  example7) empty-env init-k) 3)

(equal? (meaning-extra-credit (ast<-string-extra-credit  example8) empty-env init-k) 1)

(equal? (meaning-extra-credit (ast<-string-extra-credit  example9) empty-env init-k) 1)

(define example10 "let f = \\n\\->if n == 0 then 1 else n*f(n-1) in f(5)")

(equal? (meaning-extra-credit (ast<-string-extra-credit  example10) empty-env init-k) 120)

(define example11 "\"Hello World!\"")

(equal? (meaning-extra-credit (ast<-string-extra-credit  example11) empty-env init-k) "Hello World!")

(define example12 "[]")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example12) empty-env init-string-k) "[]")

(define example13 "null([])")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example13) init-env init-string-k) "True")

(define example14 "[1,2,3]")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example14) init-env init-string-k) "[1, 2, 3]")

(define example15 "head([1,2,3])")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example15) init-env init-string-k) "1")

(define example16 "tail([1,2,3])")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example16) init-env init-string-k) "[2, 3]")

(define example17 "let a = \\xs,ys\\->if null(xs) then ys else head(xs)::a(tail(xs), ys) in a([1,2,3],[99,100])")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example17) init-env init-string-k) "[1, 2, 3, 99, 100]")

(define example18 "let ones = 1::ones in head(ones)")

(string=? (meaning-extra-credit (ast<-string-extra-credit  example18) init-env init-string-k) "1")

