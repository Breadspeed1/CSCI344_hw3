#lang racket

;;; Author: Arthur C. Nunes


(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))


(define-tokens value-tokens (NUM ID))

(define-empty-tokens op-tokens
  (BEGIN
   END
   SEMI
   EQ1
   EQ2
   IF
   THEN
   ELSE
   OP
   CP
   COMMA
   LET
   IN
   BSLASH
   ARROW
   +
   -
   *
   /
   EOF))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (letter (:or lower-letter upper-letter))
  (digit (:/ "0" "9"))
  (ident (:+ letter))
  (number (:+ digit)))


;get-token: inputPort -> token
(define get-token
  (lexer
   ((eof) 'EOF)
   ("if" 'IF)
   ("then" 'THEN)
   ("else" 'ELSE)
   ("let" 'LET)
   ("in" 'IN)
   ("\\" 'BSLASH) ;just one, it looks like two because \ is an escape character
   ("->" 'ARROW)
   ("{" 'BEGIN)
   ("}" 'END)
   ("(" 'OP)
   (")" 'CP)
   (";" 'SEMI)
   ("," 'COMMA)
   ("=" 'EQ1)
   ("==" 'EQ2)
   ("+" '+)
   ("-" '-)
   ("*" '*)
   ("/" '/)
   (number (token-NUM (string->number lexeme)))
   (ident (token-ID (string->symbol lexeme)))
   (whitespace (get-token input-port))))


;; An expression (Exp) is one of the following.                                                              
;; a Number n                                                              
;; an Identifier x                                                                                          
;; a sum with parts e1 and e2,                                                                           
;;   where e1 and e2 are expressions  
;; a let with parts x, e1, and e2,                                                                    
;;   where x is an identifier
;;   and e1 and e2 are expressions
;; an if with parts e1, e2, and e3,
;;   where e1, e2, and e3 are expressions
;; a proc with parts x and e
;;   where x is an identifier and e is an expression
;; a funcall with parts e1 and e2
;;   where e1 and e2 are expressions
;; an assignment with parts x and e
;;   where x is an identifier and e is an expression

;; Implementation of Exp

;; Number is a Racket number                                                                                               

;; Identifier is a Racket symbol                                                                                           

; ident?: Any -> Bool                                                                                                 
(define ident? symbol?)

; (ident? 'x)                                                                                                         


(struct sum (exp1 exp2) #:transparent)

(struct diff (exp1 exp2) #:transparent)

(struct prod (exp1 exp2) #:transparent)

(struct quo (exp1 exp2) #:transparent)

(struct neg (exp) #:transparent)

(struct let-form (var exp body) #:transparent)

(struct if-form (exp1 exp2 exp3) #:transparent)

(struct proc (var exp) #:transparent)

(struct funcall (rator rand) #:transparent)

(struct assign (var exp) #:transparent)

(struct equality (exp1 exp2) #:transparent)


; make-seq goes here
(define (make-seq exp) 0) ; stub

; make-letstar goes here
(define (make-letstar defs exp) 0) ; stub

; make-curried-proc goes here
(define (make-curried-proc vars exp) 0) ; stub

; make-curried-funcall goes here
(define (make-curried-funcall exp exps) 0) ; stub



; parse-lang: (() -> token) -> SmallLangExp
(define parse-lang
  (parser
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (error 'parse-lang "error occurred, ~v ~v ~v" a b c)))
   (grammar
    (exp ((BEGIN exps END) (make-seq $2))
         ((LET let-defs IN exp) (make-letstar $2 $4))
         ((IF exp THEN exp ELSE exp) (if-form $2 $4 $6))
         ((BSLASH formals BSLASH ARROW  exp ) (make-curried-proc $2 $5))
         ((ID EQ1 exp) (assign $1 $3))
         ((comp-exp) $1))
    (exps ((exp) (list $1))
          ((exp SEMI exps) (cons $1 $3)))
    (let-def ((ID EQ1 exp) (list $1 $3)))
    (let-defs ((let-def) (list $1))
              ((let-def COMMA let-defs) (cons $1 $3)))
    (rec-def ((ID OP formals CP EQ1 exp) (list $1 (make-curried-proc $3 $6))))
    (rec-defs ((rec-def) (list $1))
              ((rec-def COMMA rec-defs) (cons $1 $3)))
    (comp-exp ((math-exp EQ2 math-exp) (equality $1 $3))
              ((math-exp) $1))
    (math-exp ((math-exp + term) (sum $1 $3))
              ((math-exp - term) (diff $1 $3))
              ((term) $1))
    (term ((term * factor) (prod $1 $3))
          ((term / factor) (quo $1 $3))
          ((factor) $1))
    (factor ((simple) $1)
            ((NUM) $1)
            ((- factor) (neg $2))
            ((simple OP actuals CP) (make-curried-funcall $1 $3)))
    (simple ((ID) $1)
            ((OP exp CP) $2))
    (actuals (() null)
             ((actualsNE) (reverse $1)))
    (actualsNE ((exp) (list $1))
               ((actualsNE COMMA exp) (cons $3 $1)))
    (formals (() null)
             ((formalsNE) (reverse $1)))
    (formalsNE ((ID) (list $1))
               ((formalsNE COMMA ID) (cons $3 $1))))))


; lexer/parser test

(define (tokens<-string str)
  (let ((i (open-input-string str)))
    (let loop ((result '())
               (tok (get-token i)))
      (if (eq? tok 'EOF)
          (reverse result)
          (loop (cons tok result) (get-token i))))))

(define (ast<-string str)
  (let ((i (open-input-string str)))
    (parse-lang (lambda () (get-token i)))))

(define example1 "2+3+4")

(equal? (ast<-string example1)
        (sum (sum 2 3) 4))




;; An environment (Env) is a Racket function from Identifier to Loc.
;; where a Loc is a NatNum

(define empty-env
  (lambda (var) (error 'empty-env "variable undefined")))

; apply-env: Env * Identifier -> Loc
(define (apply-env env var) (env var))

; extend-env: Env * Identifier * Loc -> Env
(define (extend-env env var val)
  (lambda (var2)
    (if (eq? var var2)
        val
        (env var2))))

;; A store (Store) is a list of Loc * Val
;; where a Loc is a NatNum

(define empty-store '())

; extend-store: Store * NatNum * Val -> Store
(define (extend-store store loc v) (cons (cons loc v) store))

; apply-store: Store * NatNum -> Val
(define (apply-store store loc)
  (if (null? store)
      (error 'apply-store "location undefined")
      (if (= (car (car store)) loc)
          (cdr (car store))
          (apply-store (cdr store) loc))))

;; continuations

(define init-k (lambda (v store) v))

;; denotation

(define (boolify num) (not (eqv? num 0)))


; meaning: Exp * Env * (Val * Store -> A) * Store -> A
(define (meaning exp env k store)
  (cond ((number? exp) (k exp store))
        ((ident? exp)(k (apply-store store (apply-env env exp)) store))
        ((sum? exp) 
         (meaning (sum-exp1 exp) 
                  env 
                  (lambda (v1 store2) 
                    (meaning (sum-exp2 exp) 
                             env 
                             (lambda (v2 store3) 
                               (k (+ v1 v2) store3)) 
                             store2)) 
                  store))
        ((let-form? exp) 
         (meaning (let-form-exp exp) 
                  env
                  (lambda (v store2)
                    (let ((new-loc (length store2)))
                      (meaning (let-form-body exp) 
                               (extend-env env (let-form-var exp) new-loc) 
                               k
                               (extend-store store2 new-loc v))))
                  store))
        ((if-form? exp)
         (meaning (if-form-exp1 exp) 
                  env
                  (lambda (v store2)
                    (if (boolify v)
                        (meaning (if-form-exp2 exp) env k store2)
                        (meaning (if-form-exp3 exp) env k store2)))
                  store))
        ((proc? exp) 
         (k (lambda (v k2 store2) 
              (let ((new-loc (length store2)))
                (meaning (proc-exp exp) 
                         (extend-env env (proc-var exp) new-loc) 
                         k2
                         (extend-store store2 new-loc v))))
            store))
        ((funcall? exp) 
         (meaning (funcall-rator exp) 
                  env 
                  (lambda (f store2) 
                    (meaning (funcall-rand exp) 
                             env 
                             (lambda (v store3) (f v k store3)) 
                             store2)) 
                  store))
        ((assign? exp)
         (meaning (assign-exp exp)
                  env
                  (lambda (v store2)
                    (k v (extend-store store2 (apply-env env (assign-var exp)) v)))
                  store))
        (else (error 'meaning "Unknown expression"))))

; lexer/parser/meaning test

(define example2 "2+3+4")

(equal? (meaning (ast<-string example2) empty-env init-k empty-store) 9)

(provide parse-lang sum diff quo neg let-form if-form proc funcall assign equality make-seq make-letstar make-curried-proc make-curried-funcall)
