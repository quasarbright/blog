#lang scribble/manual

Title: Understanding and Implementing Algebraic Effects
Date: 2023-10-21T16:57:20
Tags: UNLINKED, racket, continuations, tutorials, programming-languages, understand-and-implement

@require[
  scribble/example
  @for-label[racket @except-in[racket/control set] racket/generator]]
@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

Algebraic effects are kind of like exceptions that you can resume from. They can be used to express computational effects like non-determinism, generators, multi-threading, and of course, exceptions. They are a slightly less confusing alternative to using raw continuations via operators like @racket[call/cc] and have other benefits like dynamic interpretation.

In this post, we will discover and implement algebraic effects using continuations in Racket. I will assume you are familiar with Racket and continuations. If you're not, I have @link["/blog/2023/09/continuations.html"]{the perfect post for you}!

<!-- more -->

Let's start out with generators. In case you're not familiar, here's an example:

@repl[
(require racket/generator)
(define (range start stop)
  (generator ()
    (let loop ([n start])
      (unless (>= n stop)
        (displayln "yielding!")
        (yield n)
        (loop (add1 n))))))
(define g (range 1 5))
(g)
(g)
(g)
(g)
(g)
(g)
]

The function @racket[range] creates a generator that yields integers between @racket[start] and @racket[stop], excluding stop, like @racket[in-range].

The interesting thing about generators is that when we @racket[yield] a value, the body of the generator actually stops running! If this was normal code, the moment we created the generator, we'd see @racket["yielding!"] printed 4 times. But as we can see from the order of events, it only resumed the body of the generator when we asked for the next element. The control flow exits the body every time we @racket[yield] and it resumes when we fetch the next element. This is, of course, powered by continuations.

Essentially, we surround the generator body in a @racket[reset] and @racket[shift] when we yield, with some extra book-keeping and indirection to wire it all up the right way.

Now let's implement this ourselves. Our generator is going to look slightly different. We're going to build a lazy stream of values instead of a generator to make things simpler.

@repl[
(require racket/control)
(define (my-generator thnk)
  (define handler (lambda (v k) (stream-cons v (my-generator (lambda () (k (void)))))))
  (% (begin (thnk) empty-stream)
     handler))
(define (my-yield v) (fcontrol v))
(define (yield-range start stop)
  (let loop ([n 1])
    (when (<= n 4)
      (displayln "yielding")
      (my-yield n)
      (loop (add1 n)))))
(define (my-range start stop)
  (my-generator
   (lambda ()
     (yield-range start stop))))
(for ([n (my-range 1 4)])
  (displayln n))
]

@racket[%] and @racket[fcontrol] are like @racket[reset] and @racket[shift] respectively. @racket[%] establishes a handler procedure so that when @racket[fcontrol] is called, it is handled by that handler procedure. With @racket[reset] and @racket[shift], @racket[reset] doesn't really do much and @racket[shift] is where the action happens. It's the opposite with @racket[%] and @racket[fcontrol]. @racket[fcontrol] doesn't even get access to the continuation, it just bubbles some value up to the nearest @racket[%] handler and that handler controls what happens next. This is very reminiscent of exception handling, except instead of just getting the exception, the handler also gets a continuation which it can use to resume from where the exception was thrown! This is the essence of algebraic effects.

One strange thing is that in the handler, we must recursively call @racket[my-generator]. This is because @racket[%] establishes what is called a shallow hanndler. This means once an effect is performed and the body resumes from someone calling @racket[k], the handler is no longer active and needs to be re-established. So when we use @racket[k], we must recur to re-establish the handler.

Like raising an exception, the code that "performs" an effect like yielding doesn't need to know how it is going to be handled. The handling can be dynamically configured:

@repl[
(define (eager-generator thnk)
  (% (begin (thnk) (list))
     (lambda (v k) (cons v (eager-generator (lambda () (k (void))))))))
(define (eager-range start stop)
  (eager-generator
   (lambda ()
     (yield-range start stop))))
(for ([n (eager-range 1 4)])
  (displayln n))
]

By changing the handler, the behavior of the generator can be changed without having to rewrite it. Much like how the same function can run under different exception handlers.

As another example, let's implement non-determinism. Non-determinism allows us to "fork" the program to try different things without having to explicitly loop. It is useful for searches.

@repl[
(define (nondet thnk)
  (define results-rev '())
  (define (handler choices k)
    (for ([choice choices])
      (% (k choice)
         handler)))
  (% (set! results-rev (cons (thnk) results-rev))
     handler)
  (reverse results-rev))
(define (choose vs) (fcontrol vs))
(define (fail) (choose '()))
(define (assert good?) (unless good? (fail)))
(nondet
 (lambda ()
   (define a (choose (list 1 2 3 4 5 6 7 8 9 10 11 12)))
   (define b (choose (list 1 2 3 4 5 6 7 8 9 10 11 12)))
   (assert (<= a b))
   (define c (choose (list 1 2 3 4 5 6 7 8 9 10 11 12)))
   (assert (<= b c))
   (assert (= (+ (sqr a) (sqr b)) (sqr c)))
   (list a b c)))
]

In the body of the @racket[nondet], we can call @racket[choose] to make a non-deterministic choice between a list of items. This "forks" the search, running it against all choices. Even though the code will try all possible combinations, the code looks flat! We can terminate the current branch of the search by choosing between an empty list of choices. There is nothing to try, so the search terminates. If the body reaches its end, that means we found what we were looking for, so we push it onto the results list. In the end, the entire @racket[nondet] returns the list of all search matches.

The magic of the implementation is in the handler. @racket[k] is a function that resumes the body, filling in the hole of the @racket[fcontrol] with its argument. Since we want to fill in the hole with every possible choice, we loop through the choices and call @racket[k] with each choice! The rest is just book-keeping to collect all the search matches.

In our implementation of generators, to make sure we have deep effect handling (as opposed to the default shallow handling that @racket[%] and @racket[fcontrol] provides), we recurred in the handler. Our implementation of non-determinism doesn't lend itself to that kind of recursion, so instead, we call @racket[%] in the handler to re-establish only the effect handling, and not the book-keeping. There is still some recursion, as the handler passes itself to @racket[%].

Alright, let's make some abstractions!

@repl[
(define (perform v) (fcontrol v))
(define (with-effect-handler/proc their-handler thnk)
  (define (handler v k)
    (let ([k^ (lambda (v) (% (k v) handler))])
      (their-handler v k^)))
  (% (thnk)
     handler))
(define-syntax-rule
  (with-effect-handler their-handler body ...)
  (with-effect-handler/proc their-handler (lambda () body ...)))
]

@racket[perform] is just @racket[fcontrol]. @racket[with-effect-handler] is like @racket[%], but has deep effect handling instead of shallow effect handling. The way we achieved deep effect handling is slightly different here than what we did for non-determinism. Instead of using @racket[%] in the handler directly, we provide the handler with a wrapped continuation @racket[k^] which uses @racket[%] when called. This has a few benefits: if the user-supplied handler performs an effect during handling, it won't be handled by the handler that they're writing, it'll bubble out to whatever handler is outside of the whole @racket[with-effect-handler]. This is useful for situations like re-throwing exceptions. Another benefit of this is that if the @racket[k^] ends up being used outside of the dynamic extent of the @racket[with-effect-handlers], it'll re-establish effect handling as expected.

Now let's implement a crude form of exceptions using our new abstractions:

@repl[
(define (my-raise exn) (perform exn))
(define (my-with-handler/proc exn-handler thnk)
  (with-effect-handler (lambda (exn k)
                         (exn-handler exn))
    (thnk)))
(define-syntax-rule
  (my-with-handler exn-handler
    body ...)
  (my-with-handler/proc exn-handler (lambda () body ...)))
(my-with-handler (lambda (exn) "it's ok")
  (my-raise 42)
  (displayln "this print doesn't run"))
(eval:error (my-raise "oh no!"))
(my-with-handler (lambda (exn) "it's ok")
  (my-with-handler (lambda (exn) (displayln "something went wrong, re-raising") (my-raise exn))
    (my-raise 42)
    (displayln "this print doesn't run"))
  (displayln "this print doesn't run"))
]

Unhandled exceptions don't work well with this implementation because of the error you get when you use @racket[fcontrol] without a handler, but it's possibile to work around that with some of the other continuation tools.

Nice! But there is one big problem with this implementation: you can't use two effects at the same time. If we want to use non-determinism inside of a generator, it won't work because if we @racket[my-yield] inside of a @racket[non-det], the effect will be handled like a @racket[choose] effect, not a yield. How do we get around this?

One idea is to make structs for our effects and only handle effects that match a certain predicate, like what racket does for exceptions. This would work, but a much simpler way is to use continuation prompt tags. These essentially allow us to have independent delimited continuations that don't interfere with each other in the ways we're worried about. We'd just make a continuation prompt for generators and our generator handlers/operators would use that prompt tag, and the same story for other effects.

@repl[
(define (perform v [tag (default-continuation-prompt-tag)]) (fcontrol v #:tag tag))
(define (with-effect-handler/proc their-handler thnk [tag (default-continuation-prompt-tag)])
  (define (handler v k)
    (let ([k^ (lambda (v) (% (k v) handler #:tag tag))])
      (their-handler v k^)))
  (% (thnk)
     handler
     #:tag tag))
(define-syntax-rule
  (with-effect-handler tag their-handler body ...)
  (with-effect-handler/proc their-handler (lambda () body ...) tag))
(define generator-tag (make-continuation-prompt-tag 'generator))
(define-syntax-rule (my-generator body ...)
  (with-effect-handler generator-tag (lambda (v k) (stream-cons v (k (void))))
    body ...
    empty-stream))
(define (my-yield v) (perform v generator-tag))
(stream->list (my-generator (my-yield 1) (my-yield 2)))
(define nondet-tag (make-continuation-prompt-tag 'nondet))
(define-syntax-rule
  (nondet body ...)
  (let ()
    (define results-rev '())
    (with-effect-handler nondet-tag (lambda (choices k)
                                      (for ([choice choices])
                                        (k choice)))
      (set! results-rev (cons (let () body ...) results-rev)))
    (reverse results-rev)))
(define (choose vs) (perform vs nondet-tag))
(define (fail) (choose '()))
(define (assert good?) (unless good? (fail)))
(nondet
  (define a (choose (list 1 2 3 4 5 6 7 8 9 10 11 12)))
  (define b (choose (list 1 2 3 4 5 6 7 8 9 10 11 12)))
  (assert (<= a b))
  (define c (choose (list 1 2 3 4 5 6 7 8 9 10 11 12)))
  (assert (<= b c))
  (assert (= (+ (sqr a) (sqr b)) (sqr c)))
  (list a b c))
(stream->list (my-generator (nondet (my-yield (list (choose (list 1 2 3)) (choose (list 'a 'b 'c)))))))
]

Now we can use effects together!

Before we go, I want to show you one more cool example of algebraic effects: A compiler!

Let's say we're writing a compiler targeting some assembly language. Our language is very simple, only containing addition, multiplication, numbers, variables, and @racket[let]. Let's say we're trying to compile @racket[(+ (* 2 3) (* 4 5))]. In assembly, we can't perform deep calculations like this, all we can do is add the contents of two registers or multiply the contents of two registers. In other words, we can only perform operations on two immediately available numbers, no sub-computations, so we need to fully simplify the two arguments to @racket[+] before we can add them. But those two arguments could be the results of big computations too! Ideally, our program would only ever add two immediately available values. An equivalent, easier-to-compile program would look like this:

@racketblock[
(let ([a (* 2 3)])
  (let ([b (* 4 5)])
    (+ a b)))
]

In this program, the only arguments to @racket[+] and @racket[*] are numbers or variables (variables are ok, just not complex sub-computations). Fortunately, we don't have to force the users of our language to write this way, we can just translate their program into an equivalent, nicer one like this. This is called A-normal form. The idea is, if we are performing an operation and the argument is a complex expression, create a temporary variable first and replace the expression with that instead. And we do this recursively, so each addition, multiplication, or @racket[let] binding only ever has variables or constant arguments and variables are bound before use, of course. Let's write a translator:

@repl[
(define (to-anf expr)
  (define-values (expr^ bindings) (to-anf/help expr))
  (foldr (lambda (binding expr) `(let ([,(car binding) ,(cdr binding)]) ,expr))
         expr^
         bindings))
(define (to-anf/help expr)
  (match expr
    [(or (? number?) (? symbol?)) (values expr (list))]
    [`(,(and op (or '+ '*)) ,a-expr ,b-expr)
     (define-values (a-immediate a-bindings) (to-immediate a-expr))
     (define-values (b-immediate b-bindings) (to-immediate b-expr))
     (values `(,op ,a-immediate ,b-immediate) (append a-bindings b-bindings))]
    [`(let ([,x ,rhs]) ,body)
     (define-values (rhs^ rhs-bindings) (to-anf/help rhs))
     (define-values (body^ body-bindings) (to-anf/help body))
     (values body^ (append rhs-bindings
                           (list (cons x rhs^))
                           body-bindings))]))
(define (to-immediate expr)
  (match expr
    [(or (? number?) (? symbol?)) (values expr (list))]
    [_
     (define-values (expr^ bindings) (to-anf/help expr))
     (define x (gensym 'anf))
     (values x (append bindings (list (cons x expr^))))]))
(to-anf '(+ (* 2 3) (* 4 5)))
(define expr '(+ (let ([a (+ (* 2 3) (* 4 5))]) (+ (* a 6) 7)) (+ 8 9)))
(to-anf expr)
(eval expr)
(eval (to-anf expr))
]

Our translator helpers return an expression and the bindings that are necessary for it. We build up our list of bindings, carefully maintaining its order such that variables are definitely bound before use and we maintain the desired order of evaluation. Every time we encounter a complex expression that needs simplification, we replace some of that expression with variables and bind those variables outside of the expression we're building.

This is very awkward and doesn't lend itself to nice structural recursion. We can't recursively transform each sub-expression to ANF and then easily combine those two results since we need the bindings from both to combine, which involves putting part of one expression inside of another, but not the whole thing. Instead of trying to do that, we accumulate the list of bindings that will eventually surround the whole expression and at the very end, add all the @racket[let]s around it.

Here's another way to think about what's going on here: We're recursively diving into this expression to simplify it. The innermost parts of the expression will create the outermost bindings since the results of the inner expression are needed first. The innermost part of the input corresonds to the outermost part of the output, unlike typical structural recursion where the innermost part of the input corresponds to the innermost part of the output. Typical recursion doesn't combine in the way that we want, so we have to make our own little system where we keep track of bindings throughout our computation. Haskellers among you might smell a monad in the air. This is a special type of computation, like non-determinism and generators, where we could benefit from an alternative control flow using continuations.

Here is the same translation implemented using algebraic effects:

@repl[
(define anf-tag (make-continuation-prompt-tag 'anf))
(define-syntax-rule (anf body ...)
  (with-effect-handler anf-tag (lambda (v k)
                                 (match v
                                   [`(bind! ,expr)
                                    (match expr
                                      [(or (? symbol?) (? number?)) (k expr)]
                                      [_
                                       (define x (gensym 'anf))
                                       `(let ([,x ,expr]) ,(k x))])]
                                   [`(lift! ,x ,expr)
                                    `(let ([,x ,expr]) ,(k (void)))]))
    body ...))
(define (bind! expr) (perform `(bind! ,expr) anf-tag))
(define (lift! x expr) (perform `(lift! ,x ,expr) anf-tag))
(define (to-anf expr) (anf (to-anf/help expr)))
(define (to-anf/help expr)
  (match expr
    [(or (? symbol?) (? number?)) expr]
    [`(,(and op (or '+ '*)) ,a-expr ,b-expr)
     `(,op ,(to-immediate a-expr) ,(to-immediate b-expr))]
    [`(let ([,x ,rhs-expr]) ,body-expr)
     (define rhs-expr^ (to-anf/help rhs-expr))
     (lift! x rhs-expr)
     (to-anf/help body-expr)]))
(define (to-immediate expr) (bind! (to-anf/help expr)))
(to-anf '(+ (* 2 3) (* 4 5)))
(define expr '(+ (let ([a (+ (* 2 3) (* 4 5))]) (+ (* a 6) 7)) (+ 8 9)))
(to-anf expr)
(eval expr)
(eval (to-anf expr))
]

We created a new effect, @racket[bind!], which binds an expression to a variable by wrapping the result of the rest of the computation in a @racket[let] and returns the variable. If the expression is an immediate, it is returned as-is. Let's think about why this works: The leftmost innermost expression is the one which should get evaluated, and thus bound, first, which means it should be bound in the outermost let. Since the structure of our recursion matches the desired order of evaluation, the first call to @racket[bind!] will correspond to the expression that will be evaluated first. So we'll wrap the whole rest of the computation in that first @racket[let]. Remember, the "computation" here is the translation itself. The rest of the computation includes subsequent calls to @racket[bind!], which will create @racket[let] bindings inside of this outer one, as they should. The result is a lot of nested @racket[lets] with an ANF expression at the center for the final result of the supplied program.

We also have an effect @racket[lift!] which is like @racket[bind!], but we supply the variable instead of generating a fresh one. This allows us to lift a user-specified @racket[let] binding into the big nesting that we're building.

This is confusing, but it allows our code to be so much more concise. We don't have to track bindings and combine them. We don't even have to worry about bindings at all except for calling @racket[bind!] and @racket[lift!]. The rest of the code can just be structurally recursive as if there was nothing weird going on.

This idea of the effects not "polluting" or "infecting" the code around it is very special. If you've used JavaScript's promises or Haskell's monads, you've felt the pain of what is sometimes called colored code. If you need to use the result of a promise, your whole function needs to be async, which means everything that uses that function needs to be async too, and so on. This "colors" your code. JavaScript code is either async or not, and async code infects non-async code. In Haskell, monads can be similarly infectious and color your code. There is some syntactic sugar, like JavaScript's @code{async} and @code{await} and Haskell's @code{do} notation, but your code is still colored. If some part of your computation deep down needs to be async, the whole thing needs to be async.

In an ideal world, when calling a function, you shouldn't have to care if something async happens. Any effects in sub-computations should be abstracted away and not require code-changes in the caller. Algebraic effects allow us to do exactly this. They can model promises, monads, and more without coloring your code.

So should every language use algebraic effects? No, probably not. They are confusing, they make code reasoning non-local, they are hard to statically type, and multi-shot continuations like with non-determinism are not performant. In order for a language to support algebraic effects, it pretty much has to transform to CPS, which isn't right for every language. For now, it will be one of those cool features of niche languages like Racket.

Algebraic effects give us the power of continuations, wrapped up in an abstraction that lends itself to clearer code compared to raw use of @racket[call/cc], @racket[reset], @racket[shift], etc. Now you know what they are, how to use them, and how to implement them yourself.
