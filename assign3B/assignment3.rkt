#lang racket

;;; Author: Arthur C. Nunes

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM ID LIT))

(define-empty-tokens
  op-tokens
  (BEGIN END SEMI EQ1 EQ2 IF THEN ELSE OP CP COMMA LET IN BSLASH ARROW + - * / EOF OB CB DCOLON))

(define-lex-abbrevs (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (letter (:or lower-letter upper-letter))
  (digit (:/ "0" "9"))
  (ident (:+ letter))
  (number (:+ digit))
  (escape (:: "\\" (:or "\\" "\"" "n" "t" "r")))
  (string-char (:or escape (:~ #\" #\\)))
  (string-lit (:: "\"" (:* string-char) "\"")))

;get-token: inputPort -> token
(define get-token
  (lexer ((eof) 'EOF)
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
         ("::" 'DCOLON)
         ("[" 'OB)
         ("]" 'CB)
         ("+" '+)
         ("-" '-)
         ("*" '*)
         ("/" '/)
         (string-lit (token-LIT (read (open-input-string lexeme))))
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

(struct cons-form (car cdr) #:transparent)

(define (make-explicit-list lst)
  (if (null? lst)
      lst
      (cons-form (car lst) (make-explicit-list (cdr lst)))))

; make-seq goes here
(define (make-seq exp)
  (let ([edr (cdr exp)])
    (if (null? edr)
        (car exp)
        (let-form '*temp* (car exp) (make-seq edr)))))

; make-letstar goes here
(define (make-letstar defs exp)
  (if (null? defs)
      exp
      (let-form (caar defs) (cadar defs) (make-letstar (cdr defs) exp))))

; make-curried-proc goes here
(define (make-curried-proc vars exp)
  (if (null? vars)
      exp
      (proc (car vars) (make-curried-proc (cdr vars) exp))))

; make-curried-funcall goes here
(define (make-curried-funcall exp exps)
  (if (null? exps)
      exp
      (make-curried-funcall (funcall exp (car exps)) (cdr exps))))

; parse-lang: (() -> token) -> SmallLangExp
(define parse-lang
  (parser (start exp)
          (end EOF)
          (tokens value-tokens op-tokens)
          (error (lambda (a b c) (error 'parse-lang "error occurred, ~v ~v ~v" a b c)))
          (grammar
           (exp ((BEGIN exps END) (make-seq $2))
                ((LET let-defs IN exp) (make-letstar $2 $4))
                ((IF exp THEN exp ELSE exp) (if-form $2 $4 $6))
                ((BSLASH formals BSLASH ARROW exp) (make-curried-proc $2 $5))
                ((ID EQ1 exp) (assign $1 $3))
                ((comp-exp) $1))
           (exps ((exp) (list $1)) ((exp SEMI exps) (cons $1 $3)))
           (let-def ((ID EQ1 exp) (list $1 $3)))
           (let-defs ((let-def) (list $1)) ((let-def COMMA let-defs) (cons $1 $3)))
           (rec-def ((ID OP formals CP EQ1 exp) (list $1 (make-curried-proc $3 $6))))
           (rec-defs ((rec-def) (list $1)) ((rec-def COMMA rec-defs) (cons $1 $3)))
           (comp-exp ((cons-exp EQ2 cons-exp) (equality $1 $3)) ((cons-exp) $1))
           (cons-exp ((math-exp) $1) ((math-exp DCOLON cons-exp) (cons-form $1 $3)))
           (math-exp ((math-exp + term) (sum $1 $3)) ((math-exp - term) (diff $1 $3)) ((term) $1))
           (term ((term * factor) (prod $1 $3)) ((term / factor) (quo $1 $3)) ((factor) $1))
           (factor ((listexpr) (make-explicit-list $1))
                   ((simple) $1)
                   ((NUM) $1)
                   ((LIT) $1)
                   ((- factor) (neg $2))
                   ((simple OP actuals CP) (make-curried-funcall $1 $3)))
           (listexpr ((OB CB) '()) ((OB nonemptylist CB) $2))
           (nonemptylist ((exp) (list $1)) ((exp COMMA nonemptylist) (cons $1 $3)))
           (simple ((ID) $1) ((OP exp CP) $2))
           (actuals (() null) ((actualsNE) (reverse $1)))
           (actualsNE ((exp) (list $1)) ((actualsNE COMMA exp) (cons $3 $1)))
           (formals (() null) ((formalsNE) (reverse $1)))
           (formalsNE ((ID) (list $1)) ((formalsNE COMMA ID) (cons $3 $1))))))

; lexer/parser test

(define (tokens<-string str)
  (let ([i (open-input-string str)])
    (let loop ([result '()]
               [tok (get-token i)])
      (if (eq? tok 'EOF)
          (reverse result)
          (loop (cons tok result) (get-token i))))))

(define (ast<-string str)
  (let ([i (open-input-string str)]) (parse-lang (lambda () (get-token i)))))


;;; ONLY EC PART 1 WORKS
(define (ast<-string-extra-credit str) (ast<-string str))

(define example1 "2+3+4")

(equal? (ast<-string example1) (sum (sum 2 3) 4))

;; An environment (Env) is a Racket function from Identifier to Loc.
;; where a Loc is a NatNum

(define empty-env (lambda (var) (error 'empty-env "variable undefined: ~s" var)))

; apply-env: Env * Identifier -> Loc
(define (apply-env env var)
  (env var))

; extend-env: Env * Identifier * Loc -> Env
(define (extend-env env var val)
  (lambda (var2)
    (if (eq? var var2)
        val
        (env var2))))

(define prim-null 
  (lambda (arg k) 
    (arg (lambda (v) (k (null? v))))))

(define prim-head 
  (lambda (arg k) 
    (arg (lambda (v)
           (if (cons-form? v)
               (meaning (cons-form-car v) init-env k)
               (error 'head "expected cons-form"))))))

(define prim-tail 
  (lambda (arg k) 
    (arg (lambda (v)
           (if (cons-form? v)
               (meaning (cons-form-cdr v) init-env k)
               (error 'tail "expected cons-form"))))))

(define init-env 
  (extend-env 
   (extend-env 
    (extend-env empty-env 
                'null 
                (lambda (k) (k prim-null)))
    'head 
    (lambda (k) (k prim-head)))
   'tail 
   (lambda (k) (k prim-tail))))

;; A store (Store) is a list of Loc * Val
;; where a Loc is a NatNum

(define empty-store '())

; extend-store: Store * NatNum * Val -> Store
(define (extend-store store loc v)
  (cons (cons loc v) store))

; apply-store: Store * NatNum -> Val
(define (apply-store store loc)
  (if (null? store)
      (error 'apply-store "location undefined")
      (if (= (car (car store)) loc)
          (cdr (car store))
          (apply-store (cdr store) loc))))

;; continuations

(define init-k (lambda (v) v))

(define (value->string v)
  (cond
    [(number? v) (number->string v)]
    [(string? v) v]
    [(boolean? v) (if v "True" "False")]
    [(null? v) "[]"]
    [(cons-form? v) 
     (string-append "[" (cons-form->string v) "]")]
    [else (format "~a" v)]))

(define (cons-form->string cf)
  (let ([car-val (cons-form-car cf)]
        [cdr-val (cons-form-cdr cf)])
    (meaning car-val empty-env
             (lambda (v1)
               (if (null? cdr-val)
                   (value->string v1)
                   (if (cons-form? cdr-val)
                       (string-append (value->string v1) ", " (cons-form->string cdr-val))
                       (string-append (value->string v1) " :: " 
                                      (meaning cdr-val empty-env value->string))))))))

; init-string-k: continuation that converts result to string
(define init-string-k (lambda (v) (value->string v)))

;; denotation

(define (boolify num) num)

; meaning: Exp * Env * (Val -> A) -> A
(define (meaning exp env k)
  (cond
    [(number? exp) (k exp)]
    [(string? exp) (k exp)]
    [(null? exp) (k exp)]
    [(ident? exp) ((apply-env env exp) k)]
    [(neg? exp) (meaning (neg-exp exp) env (lambda (v) (k (* v -1))))]
    [(diff? exp)
     (meaning (diff-exp1 exp)
              env
              (lambda (v1)
                (meaning (diff-exp2 exp) env (lambda (v2) (k (- v1 v2)))))
              )]
    [(quo? exp)
     (meaning (quo-exp1 exp)
              env
              (lambda (v1)
                (meaning (quo-exp2 exp) env (lambda (v2) (k (/ v1 v2)))))
              )]
    [(prod? exp)
     (meaning (prod-exp1 exp)
              env
              (lambda (v1)
                (meaning (prod-exp2 exp) env (lambda (v2) (k (* v1 v2)))))
              )]
    [(sum? exp)
     (meaning (sum-exp1 exp)
              env
              (lambda (v1)
                (meaning (sum-exp2 exp) env (lambda (v2) (k (+ v1 v2)))))
              )]
    [(let-form? exp)
     (meaning (let-form-body exp) 
              (extend-env env (let-form-var exp) (lambda (k2) (meaning (let-form-exp exp) env k2)))
              k)]
    [(if-form? exp)
     (meaning (if-form-exp1 exp)
              env
              (lambda (v)
                (if (boolify v)
                    (meaning (if-form-exp2 exp) env k)
                    (meaning (if-form-exp3 exp) env k)))
              )]
    [(proc? exp)
     (k (lambda (v k2)
          (meaning (proc-exp exp)
                   (extend-env env (proc-var exp) v)
                   k2)))]
    [(funcall? exp)
     (meaning (funcall-rator exp)
              env
              (lambda (v1)
                (v1 (lambda (k2) (meaning (funcall-rand exp) env k2)) k)))]
    [(equality? exp)
     (meaning (equality-exp1 exp)
              env
              (lambda (v1)
                (meaning (equality-exp2 exp)
                         env
                         (lambda (v2) (k (if (= v1 v2) v1 #f))))))]
    [(cons-form? exp)
     (k (cons-form (cons-form-car exp) (cons-form-cdr exp)))]
    [else (display exp) (error 'meaning "Unknown expression")]))

;;; ONLY EC PART 1 WORKS
(define (meaning-extra-credit exp env k) (meaning exp env k))

; lexer/parser/meaning test

(define example2 "2+3+4")

(equal? (meaning (ast<-string example2) empty-env init-k) 9)

(provide empty-env
         init-env
         init-k
         init-string-k
         empty-store
         parse-lang
         sum
         diff
         quo
         prod
         neg
         let-form
         if-form
         proc
         funcall
         assign
         equality
         cons-form
         make-explicit-list
         make-seq
         make-letstar
         make-curried-proc
         make-curried-funcall
         meaning
         ast<-string
         value->string
         ast<-string-extra-credit
         meaning-extra-credit)
