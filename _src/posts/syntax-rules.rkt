#lang racket

;; pattern-based datum -> datum transformers with ellipses
;; limitations: ellipses may only occur at the end of a list pattern to avoid backtracking

(module+ test (require rackunit))
(require racket/hash)

;; An Expr is an S-Expression that is to be expanded by a macro

;; A Pattern is one of
;; Symbol
;; (list Pattern '...)
;; (listof Pattern)
;; Represents a specification of input syntax to a macro
;; CONSTRAINT: ellipses may only occur at the end of a list

;; A Template is one of
;; Symbol
;; (list Pattern '...)
;; (cons Pattern Pattern)
;; Represents a specification of output syntax from a macro

;; An Env is a (Hash Symbol (Rose Expr))
;; Representing a mapping from pattern variables to matched Expressions

;; A (Rose X) is one of
;; X
;; (VectorOf (Rose X))
;; We use vector to distinguish between list expressions and a collection of expressions

;; Expr (Listof Symbol) Pattern Template -> Expr
;; Expand expr according to the macro specified by literals, pattern, and template.
;; literals are matched as literal symbols in the pattern.
(define (apply-rule expr literals pattern template)
  (define env (match-pattern expr literals pattern))
  (expand-template template env))

(module+ test
  (check-equal? (apply-rule '(let ([x 1] [y 2]) (+ x y))
                            '(let)
                            '(let ([x rhs] ...) body)
                            '((lambda (x ...) body) rhs ...))
                '((lambda (x y) (+ x y)) 1 2)))

;; Expr (Listof Symbol) Pattern -> Env
;; Match the pattern against the expression
(define (match-pattern expr literals pattern)
  (define (not-literal? v) (not (member v literals)))
  (define (fail) (error 'my-syntax-rules "pattern failed to match: ~a vs ~a" pattern expr))
  (match* (pattern expr)
    [((list sub-pattern '...)
      ;; assume ... only at the end of a list to avoid backtracking
      (? list? sub-exprs))
     (define envs
       (for/list ([sub-expr sub-exprs])
         (match-pattern sub-expr literals sub-pattern)))
     (join-environments envs)]
    [((cons car-pattern cdr-pattern)
      (cons car-expr cdr-expr))
     (hash-union (match-pattern car-expr literals car-pattern)
                 (match-pattern cdr-expr literals cdr-pattern)
                 #:combine/key (lambda (k a b) (error 'my-syntax-rules "duplicate pattern var ~a" k)))]
    [((list) (list))
     (hash)]
    [((and (? symbol?) (? not-literal?)) _)
     (hash pattern expr)]
    [((not (? list?)) _)
     (if (equal? pattern expr)
         (hash)
         (fail))]
    [(_ _) (fail)]))

(module+ test
  (check-equal? (match-pattern
                 '(let ([x 1] [y 2]) (+ x y))
                 '(let)
                 '(let ([x rhs] ...) body))
                (hash 'x #(x y)
                      'rhs #(1 2)
                      'body '(+ x y))))

;; (Listof Env) -> Evn
;; Converts a list of environments into an environment of vectors,
;; combining variables' trees.
;; This is used when matching the same pattern against a bunch of expressions.
;; Assumes all envs have the same set of variables
(define (join-environments envs)
  (cond
       [(null? envs)
        (hash)]
       [else
        (define env0 (first envs))
        (define vars (hash-keys env0))
        (for/hash ([var vars])
          (values var (for/vector ([env envs])
                        (hash-ref env var (lambda () (error "var not in env"))))))]))

(module+ test
  (check-equal? (join-environments (list (hash 'x 'x 'rhs 1)
                                         (hash 'x 'y 'rhs 2)))
                (hash 'x #(x y)
                      'rhs #(1 2))))

;; Template Env -> Expr
(define (expand-template template env)
  (match template
    [(list* sub-template '... cdr-template)
     (define vars (template-vars-to-split-on sub-template env))
     (define envs (split-env env vars))
     (define expanded-repetitions
       (for/list ([env envs])
         (expand-template sub-template env)))
     (append expanded-repetitions
             (expand-template cdr-template env))]
    [(cons car-template cdr-template)
     (cons (expand-template car-template env)
           (expand-template cdr-template env))]
    [(? (lambda (var) (hash-has-key? env var)) var)
     (define bound-expr (hash-ref env var))
     (when (vector? bound-expr)
       (error 'my-syntax-rules "missing ellipses for pattern variable ~a in template" var))
     bound-expr]
    [lit lit]))

(module+ test
  (check-equal? (expand-template '((lambda (x ...) body) rhs ...)
                                 (hash 'x #(x y)
                                       'rhs #(1 2)
                                       'body '(+ x y)))
                '((lambda (x y) (+ x y)) 1 2)))

;; Template Env -> (Listof Symbol)
;; variables in the template that are bound to repetitions.
;; output list contains no duplicates.
(define (template-vars-to-split-on template env)
  (define repetition-vars
    (for/list ([var (hash-keys env)]
               #:when (vector? (hash-ref env var)))
      var))
  (define template-vars (get-template-vars template))
  ;; intersection
  (for/list ([var repetition-vars]
             #:when (member var template-vars))
    var))

(module+ test
  (check-equal? (template-vars-to-split-on '((lambda (x ...) body) rhs ...)
                                           (hash 'x #(x y)
                                                 'rhs #(1 2)
                                                 'body '(+ x y)))
                '(x rhs)))

;; Template -> (Listof Symbol)
;; get variables referenced in template. May include literal symbols.
(define (get-template-vars template)
  (match template
    [(list* sub-template '... cdr-template)
     (append (get-template-vars sub-template)
             (get-template-vars cdr-template))]
    [(cons car-template cdr-template)
     (append (get-template-vars car-template) (get-template-vars cdr-template))]
    [(list) (list)]
    [(? symbol? var) (list var)]))

;; Env (Listof Symbol) -> (Listof Env)
;; Split an environment of vectors into a list of environments.
;; Creates one environment per repetition.
(define (split-env env repetition-vars)
  (define repetition-count (get-repetition-count env repetition-vars))
  (for/list ([repetition-index (in-range repetition-count)])
    (for/fold ([env env])
              ([var repetition-vars])
      (hash-set env var (vector-ref (hash-ref env var) repetition-index)))))

(module+ test
  (check-equal? (split-env (hash 'x #(x y)
                                 'rhs #(1 2)
                                 'body '(+ x y))
                           '(x rhs))
                (list (hash 'x 'x
                            'rhs 1
                            'body '(+ x y))
                      (hash 'x 'y
                            'rhs 2
                            'body '(+ x y)))))

;; Env (Listof Symbol) -> Natural
;; How many times are the repetition-vars repeated?
;; Errors if there are different repetition lengths
(define (get-repetition-count env repetition-vars)
  (when (null? repetition-vars)
    (error 'my-syntax-rules "too many ellipses in template"))
  (define repetition-counts
    (for/list ([var repetition-vars])
      (vector-length (hash-ref env var))))
  (define maximum (apply max repetition-counts))
  (define minimum (apply min repetition-counts))
  (unless (= maximum minimum)
    (error 'my-syntax-rules "incompatible ellipsis match counts for template"))
  minimum)

(module+ test
  (check-equal? (get-repetition-count (hash 'x #(x y)
                                            'rhs #(1 2)
                                            'body '(+ x y))
                                      '(x rhs))
                2)
  (check-exn
   #rx"incompatible ellipsis match counts for template"
   (lambda ()
     (get-repetition-count (hash 'x #(x y z)
                                 'rhs #(1 2)
                                 'body '(+ x y))
                           '(x rhs))))
  (check-exn
   #rx"too many ellipses in template"
   (lambda ()
     (get-repetition-count (hash 'x #(x y z)
                                 'rhs #(1 2)
                                 'body '(+ x y))
                           '()))))
