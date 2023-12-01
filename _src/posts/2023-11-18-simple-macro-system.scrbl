#lang scribble/manual

Title: Understanding and Implementing a Macro System
Date: 2023-11-18T10:28:40
Tags: UNLINKED, racket, tutorials, programming-languages, understand-and-implement


@require[
  scribble/example
  @for-label[racket racket/hash]]

@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

Macros are a powerful tool that allow programmers to extend the syntax of a language. In a language with macros, features like for-loops, while-loops, and pattern matching can be implemented as a library by users of the langauge! In this post, we'll discover what macros are, how and why to use them, and how to implement a tiny language with a simple macro system.

For this post, you'll need some familiarity with Racket, but no familiarity with macros is required. If you don't know what something is, click on the variable name in the code and you'll be taken to its documentation.

<!-- more -->

Let's say you want to swap two variables, @racket[x] and @racket[y]. This is how we'd do it normally:

@repl[
(define x 1)
(define y 2)
(define tmp #f)
(set! tmp x)
(set! x y)
(set! y tmp)
x
y
]

If we did this often, it'd get pretty annoying to have to do the same thing every time. We should try to abstract it! Let's write a function, @racket[swap], to do it for us:

@repl[
(define (swap! x y)
  (define tmp #f)
  (set! tmp x)
  (set! x y)
  (set! y tmp))
(define x 1)
(define y 2)
(swap! x y)
x
y
]

It didn't work! This is because when you mutate an argument to a function, it doesn't affect the variable you passed in. If we passed in some mutable structure like a @racket[box], we could mutate the box, but that's not we want. We want to mutate variables. Is there a way to pass in a variable itself? Sort of, but not with regular old functions like this.

If you like to make abstractions to avoid repeating yourself, you've likely hit this sort of wall before. You want to make some abstraction, but functions aren't enough for what you need.

One way to solve this problem is to dynamically generate code. For example, we could leave a comment like @code{; rewrite (swap! x y)} and make a special pre-processor that detects comments like this and replaces them with the code they should generate.

That would work, but it sounds very messy. We'd need to make a parser for Racket programs, a parser for our special comments, and we'd have to implement all of these rewrite rules in the pre-processor. But what if this pre-processor was integrated into the language itself? After all, Racket already has a parser for Racket programs!

This is basically what a macro system is. Macros are essentially rewrite rules that rewrite the code before it runs. Racket, of course, already has macros:

@repl[
(define-syntax-rule
  (swap! x y)
  (let ([tmp #f])
    (set! tmp x)
    (set! x y)
    (set! y tmp)))
(define x 1)
(define y 2)
(swap! x y)
x
y
]

We define macros with something like @racket[define-syntax-rule] and we use them with the same syntax as function calls.

Macros are great for little rewrite rules like this, but they are also good for more complicated syntactic abstractions. For example, let's pretend Racket doesn't have any looping mechanisms. How would we implement while loops?

@racketblock[
(define iterations 3)
(while (> iterations 0)
  (set! iterations (- iterations 1))
  (displayln "loop"))
]

One easy way to do this would be recursion:

@repl[
(define iterations 3)
(define (go)
  (when (> iterations 0)
    (set! iterations (- iterations 1))
    (displayln "loop")
    (go)))
(go)
]

When you do a while loop like @racket[(while condition body ...)] using recursion, it will generally look like this:

@racketblock[
(define (go)
  (when condition
    body
    ...
    (go)))
(go)
]

If the condition is truthy, we run the body and then recur to loop again. If it's not, we'll just stop looping and return nothing. And we need to kick the whole thing off with a call to @racket[go].

The actual implementation of the macro looks exactly like what we wrote:

@repl[
(define-syntax-rule
  (while condition body ...)
  (let ()
    (define (go)
      (when condition
        body
        ...
        (go)))
    (go)))
(define iterations 3)
(while (> iterations 0)
  (set! iterations (- iterations 1))
  (displayln "loop"))
]

The @racket[(let () ...)] is just a little trick to allow us to use @racket[define] and multiple expressions in the result of our macro.

This is pretty cool, and a little more complicated than our @racket[swap!] macro, but it's still just scratching the surface of what macros are capable of. We can implement entire domain specific languages with macros like @racket[match] and @racket[class], and we can even embed entire programming languages like @hyperlink["http://minikanren.org/"]{mini kanren}.

Now that we know what macros are, let's implement them! To do this, we're going to make a little programming language that has a macro system. They won't be as nice as Racket's macro system, but they'll give a sense of how macros work.

Before we start doing anything fancy with macros, let's build a regular old interpreter for the lambda calculus.

We are, of course, going to be using s-expressions as syntax. For those unfamiliar, an s-expression is either an atom, like a variable name or a number, or a parentheses-enclosed list of s-expressions. This simple representation makes syntax very easy to manipulate as data, which is exactly what our macros will end up doing.

For example, the expression @racket[(add1 1)] is represented as @racket[(list 'add1 1)]. The first element of the list is a symbol, @racket['add1], which represents a variable name. The second element is the number @racket[1]. Numbers just represent themselves.

To create s-expressions, we can use @racket[quote]:

@repl[
(eval:alts (#,(racketkeywordfont "quote") (add1 1)) '(add1 1))
(list 'add1 1)
]

This is such an important function, there is special syntax for it:

@repl[
'(add1 1)
]

@racket[quote] takes in an expression and returns the expression itself as data instead of evaluating it.

Another useful tool for building s-expressions is @racket[quasiqute] and @racket[unquote]:

@repl[
(list 'add1 3)
(list 'add1 (+ 1 2))
(eval:alts (#,(racketkeywordfont "quasiquote") (add1 (#,(racketkeywordfont "unquote") (+ 1 2)))) (quasiquote (add1 (unquote (+ 1 2)))))
]

@racket[quasiquote] is like @racket[quote], but we can escape the quotation with @racket[unquote] to actually evaluate an expression instead of just putting it right in the output as-is. This is sort of like format strings in Python or template literal strings in JavaScript, but for making s-expressions instead of strings.

Of course, this has a shorthand too:

@repl[
`(add1 ,(+ 1 2))
]

In our little language, macros will be functions that take in an expression and return an s-expression. We'll call these functions transformers. Like in Racket, macro usages will look just like function calls. But when a function call expression is a macro usage, instead of evaluating the function and arguments and passing them to the function, we'll pass the whole macro usage expression to the transformer function.

For example, here's what @racket[swap!] will look like:

@racketblock[
(let-macro ([swap! (lambda (expr)
                     (let ([tmp (gensym 'tmp)])
                       `(let ([,tmp ,(second expr)])
                          (begin (set! ,tmp ,(second expr))
                                 (set! ,(second expr) ,(third-expr))
                                 (set! ,(third-expr) ,tmp)))))])
  (let ([x 1])
    (let ([y 2])
      (begin (swap! x y)
             (displayln x)
             (displayln y)))))
]

The transformer takes in the expression @racket[(swap! x y)], which is a list of three elements. The second and third are @racket[x] and @racket[y] respectively. The first is just @racket[swap!]. These types of macros are very annoying to write, which is one of the reasons why we have tools like @racket[define-syntax-rule].

For now, we'll start out by making an interpreter for a language with just @racket[lambda] expressions, function applications, variables, and constants like numbers. I'll assume you're somewhat familiar with lambda calculus interpreters and won't explain it in depth:

@repl[
(require racket/hash)
(define (eval-expr expr env)
  (match expr
    [(? symbol? x) (hash-ref env x (lambda () (error 'eval-expr "unbound variable ~a" x)))]
    [`(lambda ,argnames ,body)
     (lambda args
       (unless (= (length argnames) (length args)) (error 'eval-expr "arity error"))
       (define arg-env (for/hasheq ([argname argnames] [arg args]) (values argname arg)))
       (eval-expr body (hash-union env arg-env)))]
    [(cons fun-expr arg-exprs)
     (apply (eval-expr fun-expr env) (for/list ([arg-expr arg-exprs]) (eval-expr arg-expr env)))]
    [atom atom]))
(define (eval-top expr) (eval-expr expr (hasheq)))
(eval-top 1)
(eval-top '((lambda (x) x) 1))
]

@racket[match] has @racket[quasiquote] patterns, which match quoted forms as their s-expressions and unquoted forms as patterns.

We're using a @racket[hasheq] to represent our runtime environment mapping variables to values. We're also using Racket functions to represent our functions, which is kind of cheating. But we're not interested in writing a lambda calculus interpreter, we're interested in writing a macro system! So we'll leverage as much of Racket's interpreter as we can.

Let's also add some built-in functions and special forms:

@repl[
(define built-ins (hasheq '* *
                          '+ +
                          'list list
                          'cons cons
                          'equal? equal?
                          'first first
                          'second second
                          'third third
                          'gensym gensym
                          'displayln displayln))
(define (eval-expr expr env)
  (match expr
    [(? symbol? x) (hash-ref env x (lambda () (error 'eval-expr "unbound variable ~a" x)))]
    [`(lambda ,argnames ,body)
     (lambda args
       (unless (= (length argnames) (length args)) (error 'eval-expr "arity error"))
       (define arg-env (for/hasheq ([argname argnames] [arg args]) (values argname arg)))
       (eval-expr body (hash-union env arg-env)))]
    [`(if ,condition-expr ,then-expr ,else-expr)
     (if (eval-expr condition-expr env)
         (eval-expr then-expr env)
         (eval-expr else-expr env))]
    [`(let ([,x ,rhs]) ,body)
     (eval-expr body (hash-set env x (eval-expr rhs env)))]
    [(cons 'begin exprs)
     (last (cons (void) (for/list ([expr exprs]) (eval-expr expr env))))]
    [(list 'quote expr) expr]
    [(cons fun-expr arg-exprs)
     (apply (eval-expr fun-expr env) (for/list ([arg-expr arg-exprs]) (eval-expr arg-expr env)))]
    [atom atom]))
(define (eval-top expr) (eval-expr expr built-ins))
(eval-top '(if #t (+ 1 2) 42))
(eval-top '(let ([x (+ 1 2)]) (list x x)))
(eval-top '(list 1 (quote (add1 x))))
]

Since we're representing functions as Racket functions, we can just put Racket functions into our initial environment to add built-ins.

@racket[if], @racket[let], and @racket[begin] are unsurprising. @racket[quote] is a little subtle. For @racket[quote], we just return @racket[expr] without evaluating it. Also, we used a list pattern, but we could've used @code{`(quote ,expr)} or even @code{`',expr}. I just decided to use a list pattern to avoid confusion, but these three patterns are equivalent.

You might be wondering how we can use the apostrophe shorthand in our language. The racket parser (technically, the reader) translates @code{'expr} into @code{(quote expr)} before macros expand or code runs (macro expansion refers to macro usages being re-written). So in the last example, our interpreter actually ends up getting the s-expression @racket[(list 'list 1 (list 'quote (list 'add1 'x)))].

Now let's implement @racket[quasiquote]:

@repl[
(define (eval-expr expr env)
  (match expr
    [(? symbol? x) (hash-ref env x (lambda () (error 'eval-expr "unbound variable ~a" x)))]
    [`(lambda ,argnames ,body)
     (lambda args
       (unless (= (length argnames) (length args)) (error 'eval-expr "arity error"))
       (define arg-env (for/hasheq ([argname argnames] [arg args]) (values argname arg)))
       (eval-expr body (hash-union env arg-env)))]
    [`(if ,condition-expr ,then-expr ,else-expr)
     (if (eval-expr condition-expr env)
         (eval-expr then-expr env)
         (eval-expr else-expr env))]
    [`(let ([,x ,rhs]) ,body)
     (eval-expr body (hash-set env x (eval-expr rhs env)))]
    [(cons 'begin exprs)
     (last (cons (void) (for/list ([expr exprs]) (eval-expr expr env))))]
    [(list 'quote expr) expr]
    [(list 'quasiquote expr) (eval-quasiquote expr env)]
    [(cons fun-expr arg-exprs)
     (apply (eval-expr fun-expr env) (for/list ([arg-expr arg-exprs]) (eval-expr arg-expr env)))]
    [atom atom]))
(define (eval-quasiquote expr env)
  (match expr
    [(list 'unquote expr) (eval-expr expr env)]
    [(? list? exprs) (for/list ([expr exprs]) (eval-quasiquote expr env))]
    [_ expr]))
(eval-top '`(+ 1 ,(+ 1 2)))
]

We recursively go through the expression's children searching for @racket[unquote] and evaluating those expressions when we find them. Without @racket[unquote]s, we end up reproducing the original expression. This is a much simpler, less powerful version of Racket's @racket[quasiquote], but it'll suffice for us.

Now, we're ready to implement macros!

@repl[
(struct transformer [fun] #:transparent)
(define (eval-expr expr env)
  (match expr
    [(? symbol? x) (hash-ref env x (lambda () (error 'eval-expr "unbound variable ~a" x)))]
    [`(lambda ,argnames ,body)
     (lambda args
       (unless (= (length argnames) (length args)) (error 'eval-expr "arity error"))
       (define arg-env (for/hasheq ([argname argnames] [arg args]) (values argname arg)))
       (eval-expr body (hash-union env arg-env)))]
    [`(if ,condition-expr ,then-expr ,else-expr)
     (if (eval-expr condition-expr env)
         (eval-expr then-expr env)
         (eval-expr else-expr env))]
    [`(let ([,x ,rhs]) ,body)
     (eval-expr body (hash-set env x (eval-expr rhs env)))]
    [(cons 'begin exprs)
     (last (cons (void) (for/list ([expr exprs]) (eval-expr expr env))))]
    [(list 'quote expr) expr]
    [(list 'quasiquote expr) (eval-quasiquote expr env)]
    [`(let-macro ([,x ,transformer-expr]) ,body)
     (eval-expr body (hash-set env x (transformer (eval-expr transformer-expr env))))]
    [(cons fun-expr arg-exprs)
     (define fun (eval-expr fun-expr env))
     (if (transformer? fun)
         (eval-expr ((transformer-fun fun) expr) env)
         (apply fun (for/list ([arg-expr arg-exprs]) (eval-expr arg-expr env))))]
    [atom atom]))
(eval-top '(let-macro ([one (lambda (expr) 1)]) (one)))
(eval-top '(let-macro ([describe (lambda (expr) `(list (quote ,(second expr)) ,(second expr)))]) (describe (+ 1 2))))
]

First, we made a struct for our transformers so we can distinguish between regular functions and macros. And to create macros, we have a different version of @racket[let] called @racket[let-macro] that turns the value into a transformer before binding it to the variable. Then, all we had to do was modify the application case to check if the function is a transformer or not. If it is a transformer, we apply it to the entire expression, which produces another expression. Finally, we evaluate the resulting expression. That's it! Now we have macros.

Racket's macro system is much more sophisticated than the one we implemented here, of course. Unlike our language, Racket expands all macros away before any of the code runs. Another nice thing Racket's macro system does is handle variable collisions for you. Remember how we made a @racket[tmp] variable in our @racket[swap!] macro? What if we did @racket[(swap! tmp y)]? It'd break, right? Actually, no:

@repl[
(define tmp 1)
(define y 2)
(swap! tmp y)
tmp
y
]

Racket's macro system has a concept of macro hygiene where you don't have to worry about these kinds of variable collisions. It just takes care of it for you. How it works is really cool and worth its own post, so I won't explain it here. But in our language, to avoid this problem, we have to use @racket[gensym] to avoid user-supplied variables colliding with macro-introduced variables or vice-versa. @racket[gensym] creates a unique symbol, so we don't have to worry about collisions.

Speaking of @racket[swap!], unfortunately, the language we made doesn't have mutable variables, so we can't implement it. But if we had mutable variables, the code we wrote a while ago for it would work in our language. I chose not to implement mutable variables in this post because it complicates the interpreter and I wanted to keep it simple so we could focus on the macro system.

One interesting thing about our language is that macros are defined and expanded dynamically, rather than before the program runs like in Racket. Transformers are also evaluated against the current environment, which means they have access to local variables. In a language where expansion happens before the code runs, you wouldn't be able to use local variables in the transformer directly.

Macros are very cool, and there has been a lot of work on developing powerful, easy-to-use macro systems over the years. This post just scratches the surface of what's possible, but hopefully it gave some understanding of how a macro system works and why they're so useful.
