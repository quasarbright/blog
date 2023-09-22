#lang scribble/manual

Title: Continuations
Date: 2023-09-16T15:41:55
Tags: racket, tutorials, programming-languages, DRAFT

@require[
  scribble/example
  @for-label[racket]]
@(define eval (make-base-eval '(require racket)))

@(define callcc (racket call-with-current-continuation))

Continuations are a powerful tool that allow you to implement control flow constructs like exceptions, generators, and multi-threading, and back tracking as libraries. That's right, libraries! In a programming language that gives access to continuations, these features don't have to be baked into the implementation of the language. In this post, we will explore what continuations are, how to use them, and how to implement them in a programming language as a pre-processing step. foo


<!-- more -->

@table-of-contents[]


This post is written in Racket since it has first class continuations, but I'll explain Racket-y stuff as we go, so you don't need to know it.

@section{What is a Continuation?}

A continuation captures the context of evaluation of an expression. To make this concrete, let's start with an example borrowed from @hyperlink["https://docs.racket-lang.org/guide/conts.html"]{The Racket Guide}:

@examples[
  #:label #f
  #:eval eval
  (+ 1 (+ 1 (+ 1 2)))
]

A brief Racket aside: Instead of writing @code{1 + 1} in Racket, we write @racket[(+ 1 1)]. There are a lot of parentheses!

At the point where the @racket[2] is being evaluated, we are "inside" of three enclosing addition expressions. The next steps for evaluation are to evaluate the expression @racket[2] and then add 3 to it.

Ok, so where do continuations come in? A continuation captures the "and then add 3 to it". At the point where the @racket[2] is being evaluated, the current continuation is something that adds 3. Evaluation has this continuation, evaluates the expression @racket[2] to the value 2, and then plugs in 2 to the current continuation, giving us 5.

We can think of a continuation like a hole in the program: @racket[(+ 1 (+ 1 (+ 1 ?)))]. This is the context of evaluation at the point where we are evaluating the @racket[2]. In fact, Racket allows us to do exactly this!

@examples[
#:label #f
#:eval eval
(+ 1 (+ 1 (+ 1 (call-with-current-continuation (lambda (k) (k 2))))))
]

Some Racket stuff: @racket[lambda] is used for creating anonymous functions, so @racket[(lambda (x) (* 5 x))] is a function that multiplies @racket[x] by 5. Functions are called like @racket[(f x y)] instead of @code{f(x,y)}. When we write @racket[(+ 1 2)], we are calling the @racket[+] function with two arguments. @racket[call-with-current-continuation] is a special built-in function that I'll explain soon, and we're calling it with one argument, our lambda function. Our lambda @racket[(lambda (k) (k 2))] takes in a function, @racket[k], and calls it with the argument @racket[2]. There are a lot of functions here, so it's easy to get confused. But it'll become more clear with some examples.

@racket[call-with-current-continuation] is a built-in function that gives us access to the current continuation. Using it pretty much always looks like this: @racket[(call-with-current-continuation (lambda (k) do-something-with-k))], where @racket[k] is the current continuation. The continuation @racket[k] is represented as a 1-argument function, which "fills in the question mark" and continues evaluating that expression. By convention, we use @racket[k] as a variable name for continuations.

This is cool, but it looks like we haven't really gained anything. The result is the same, so why are we making this more complicated by involving continuations? The real power comes from the fact that we can do whatever we want with @racket[k]:

@examples[
#:label #f
#:eval eval
(define saved-k #f)
(define (save-it!)
  (call-with-current-continuation
    (lambda (k) ; k is the captured continuation
      (set! saved-k k)
      (k 2))))
(+ 1 (+ 1 (+ 1 (save-it!))))
]

Some Racket stuff: @racket[define] is used to define variables and functions. @racket[#f] is false, and we use as an initial value for the saved continuation, @racket[saved-k]. We define @racket[save-it!] to be a 0-argument function which uses @racket[call-with-current-continuation]. @racket[set!] (pronounced set bang) is a variable assignment, so this sets the value of @racket[saved-k] to be @racket[k], which is the current continuation that we're capturing. There is nothing special about the exclamation point in the name, it is just a convention for functions that have side effects. When there are multiple expressions in the body of a @racket[lambda] expression, we evaluate them in order and return the last one. So this lambda assigns @racket[saved-k] to the current continuation and then "fills the hole" with @racket[(k 2)].

Instead of just getting the current continuation and immediately using it by "filling in the hole" with 2, we save it to the variable @racket[saved-k] first. We still get 5, but now, we have access to that saved continuation:

@examples[
#:label #f
#:eval eval
(saved-k 2)
(saved-k 6)
(saved-k 10)
]

We can "resume" the computation as many times as we want using the continuation we captured and saved. Pretty cool!

The continuation doesn't just have to be for the last thing that gets evaluated in an expression either. It also captures everything that happens "after the hole":

@examples[
#:label #f
#:eval eval
(+ 1 (+ 1 (+ 1 (* (save-it!) 4))))
]

Now, instead of just adding three to what we fill in the hole with, we're multiplying that value by four and then adding three:

@examples[
#:label #f
#:eval eval
(saved-k 5)
]

One weird thing about continuations created with @racket[call-with-current-continuation] is that they "abort" when you use them. For example:

@examples[
#:label #f
#:eval eval
(* (saved-k 5) (saved-k 7))
]

When we use @racket[saved-k], we abort the multiplication and return the result of resuming the continuation. We never compute @racket[(saved-k 7)]. This can be a problem for certain applications of continuations, and we'll address it later.

Another detail is that if we don't use @racket[k] at all when we use @racket[call-with-current-continuation], the result is whatever the lambda returns. For example:

@examples[
#:label #f
#:eval eval
(+ 1 (call-with-current-continuation (lambda (k) 2)))
]

Let's recap what we know so far: @racket[call-with-current-continuation] creates a hole in an expression, a continuation given to us as the function @racket[k], that, when called with an argument, gives us the result of evaluating the expression if you replaced the hole with that argument. Calling @racket[k] also aborts evaluation of the expression you called it in with the result of resuming the continuation.

@section{Using Continuations}

Now let's implement some control flow constructs.

@subsection{Early Return}

To get a taste for how we can use continuations for making our own control flow constructs, we can implement an early return operation.

For example, let's write a function that computes the product of a list of numbers:

@examples[
#:label #f
#:eval eval
(define (product nums)
  (if (empty? nums)
      1
      (* (first nums) (product (rest nums)))))
(product (list 2 3 4))
]

Some Racket stuff: In Racket, @racket[if] is an expression instead of a statement. It's like the ternary conditional operator in popular languages. @racket[empty?] (pronounced empty huh) is a function that returns true when its argument is an empty list. Again, the question mark in the name isn't anything special, it's just a convention for a function that returns a boolean. In Racket, we use linked lists, where @racket[(first nums)] returns the first element and @racket[(rest nums)] returns the rest of the list. For @racket[(list 2 3 4)], the @racket[first] is @racket[2] and the @racket[rest] is @racket[(list 3 4)]. When a list only has one element, the @racket[rest] is an empty list.

This ends up producing the multiplication @racket[(* 2 (* 3 (* 4 1)))].

Let's think about what happens for @racket[(product (list 2 3 0 4))]. We'll end up computing @racket[(* 2 (* 3 (* 0 (* 4 1))))], which will be zero. But this is a waste. We don't need to compute any of those multiplications because once we see a zero, the whole answer will be zero. It would be nice if we could just instantly return 0 without having to do any multiplications. We can avoid the @racket[(* 0 (* 4 1))] by adding a case that returns 0 if the current number is zero instead of recurring, but then we'd still end up computing @racket[(* 2 (* 3 0))], which is a waste. What we want is to skip the pending recursive calls and immediately return 0 for the whole thing.

We can do this using @racket[call-with-current-continuation]:

@examples[
#:label #f
#:eval eval
(define (product-helper nums abort-k)
  (if (empty? nums)
      1
      (if (equal? (first nums) 0)
          (abort-k 0)
          (* (first nums) (product-helper (rest nums) abort-k)))))
(define (product nums)
  (call-with-current-continuation (lambda (k) (product-helper nums k))))
(product (list 2 3 4))
(product (list 2 3 0 4))
(product (list 2 "not a number" 4 0))
]

We moved the core logic to a helper function, @racket[product-helper], which takes in an additional argument, @racket[abort-k], which is a continuation used for returning early. We call this continuation to immediately return 0 when we find a zero in the list. We can see that this really does avoid the multiplications from the third example with the string, since we would get an error if we multiplied a string with a number.

Here, the continuation captured by @racket[k] is for the entire product computation. In the previous examples, we've been in the middle of a computation (nested addition expressions) and used @racket[call-with-current-continuation] to capture the surrounding context so we can re-use it with @racket[saved-k]. But here, we're doing the opposite: We're capturing the continuation of the whole product computation, and within the computation of @racket[product], we're using the continuation to throw away the surrounding context and replace the whole computation with an immediate answer. This throws away the pending multiplications from surrounding recursive calls, saving us from computing them.

@subsection{Exceptions}

This behavior of aborting a computation early sounds a lot like exceptions. In fact, we can implement a crude form of exceptions using this same trick!

@examples[
#:label #f
#:eval eval
(define throw-k #f)
(define (with-exceptions body)
  (call-with-current-continuation
    (lambda (k)
      (set! throw-k k)
      (body))))
(with-exceptions (lambda () "all good"))
(with-exceptions (lambda () (+ 1 (throw-k "something went wrong"))))
]

@racket[(lambda () "all good")] creates a zero-argument function which returns "all good". We use zero-argument functions here to delay the evaluation of the body so we can wrap the continuation logic around it.

This doesn't support handlers or nested @racket[with-exceptions] blocks, but that's possible with some careful book-keeping.

@subsection{Generators}

Let's implement generators. For those unfamiliar, here is an example using Racket's generators:

@examples[
#:label #f
#:eval eval
(require racket/generator)
(define g (generator () (yield 1) (println "hello") (yield 2) (yield 3)))
(g)
(g)
(g)
(g)
(g)
]

A generator is a type of sequence whose elements come from calls to @racket[yield] in the body of the @racket[generator] expression. Calling @racket[(g)] gives us the next element @racket[yield]ed by the generator. Once there are no more elements, @racket[(g)] returns nothing. Also notice that we only print @racket["hello"] after the second call. This means that the body of the generator is suspended after each yield, and the elements are only produced on-demand. This allows us to create generators of infinite sequences:

@examples[
#:label #f
#:eval eval

(define g (generator () (define (loop n)
                          (yield n)
                          (loop (+ 1 n)))
                        (loop 1)))
(g)
(g)
(g)
(g)
(g)
]

Now let's implement generators:

@examples[
#:label #f
#:eval eval
(define yield-k #f)
(define resume-k #f)
(define (generator body)
  (set! resume-k (lambda (val) (body)))
  (lambda ()
    (call-with-current-continuation
      (lambda (k)
        (set! yield-k k)
        (resume-k (void))))))
(define (yield val)
  (call-with-current-continuation
    (lambda (k)
      (set! resume-k k)
      (yield-k val))))
(define g (generator (lambda () (yield 1) (yield 2))))
(g)
(g)
(g)
(g)
]

@; TODO instead of needing 2 conts, you can just use delimited continuations. then you get to explain that too.

There is a lot going on here, so let's go through every piece:

Firstly, unlike the previous examples, we need two saved continuations. This is because when we "jump out" and yield, we need to be able to jump back in. Previously, we only ever jumped out. @racket[yield-k] keeps track of where to "jump out" to, and @racket[resume-k] keeps track of where to "jump back in" to.

Focusing on @racket[yield-k], like before, we set the "jump out" continuation @racket[yield-k] in a @callcc in our entry point, @racket[generator]. And when we @racket[yield], we use @racket[yield-k] to jump out. But now, this is all happening in a lambda. In particular, it is happening in the zero-argument lambda that is returned by @racket[generator]. So when we yield a value, it's like we're replacing that lambda body with the value we're yielding. And this is exactly what we want, since calling the function returned by @racket[generator] should return the next yielded value.

Now let's talk about @racket[resume-k]. This variable keeps track of the continuation of the last call to yield. In other words, it is the continuation that, when called, resumes the body of the generator after it got suspended when it last yielded. As such, we initialize it to just run the whole body at the start, since the body hasn't run yet. Then, every time we yield, we update @racket[resume-k] to the current continuation and we jump out with the value that was yielded. Then, next time the generator is called, we will resume using the continuation we just saved and run until we reach the end of the body or yield again. We call @racket[resume-k] with @racket[(void)] since the result of evaluating a @racket[yield] expression in the body should be nothing.

To make sure we understand all the moving parts, let's go through our example step by step:

Both continuations start out as @racket[#f]. Then, we make the generator @racket[g]. This sets @racket[resume-k] to a function that just runs the body. It's not really a continuation, but that's ok. We then return the generator, which is that zero-argument lambda.

Then, we call the generator function. This sets @racket[yield-k] to the current continuation, which will replace the entire expression @racket[(g)], and resumes the body, which will just start the body. Then, the body yields 1, which sets @racket[resume-k] to the current continuation in the body, which is a function that will end up calling @racket[(yield 2)], and then we jump out by calling @racket[(yield-k 1)], which replaces the @racket[(g)] with 1.

Now, we call @racket[(g)] again. This again sets @racket[yield-k] to the current continuation, which will replace the entire expression @racket[(g)], and resumes the body, which will resume after the first yield. Then, the body yields 2, which sets @racket[resume-k] to the current continuation in the body, which is a function that will end up doing nothing since the body is finished, and then we jump out by calling @racket[(yield-k 2)], which replaces the @racket[(g)] with 2.

Now, we call @racket[(g)] one more time. The same thing happens to @racket[yield-k] as before, and we resume the body, which immediately returns void because the body is done.

The final call does the same thing.

The reality is a little more complicated when you start to think about the fact that these continuations are aborting and about how much context they're actually capturing, but that's not important for our purposes.

@subsection{Multi Threading}

For multi threading, we'll support the following operations:

@racket[(spawn k)] puts a thread with continuation @racket[k] into the thread queue.

@racket[(quit)] kills the current thread and removes it from the thread queue.

@racket[(yield)] hands control from the current thread to another thread.

@racket[(start-threads)] starts executing threads in the thread queue.

@racket[(halt)] exits all threads.

@#reader scribble/comment-reader
(examples #:label #f #:eval eval

; list of continuations that resume their thread
(define threads (list))
; continuation to escape to the scheduler. aborts the computation.
(define quit-k #f)

; kicks off the thread scheduler and blocks until threads are completed.
(define (start-threads)
  (call-with-current-continuation
   (lambda (k)
     (set! quit-k k)))
  ; scheduler
  (unless (empty? threads)
    (define next-thread (first threads))
    (set! threads (rest threads))
    (next-thread (void))))

; add a thread to the end of the queue
(define (push-thread! k)
  (set! threads (append threads (list k))))

; puts a thread for body in the thread queue
(define (spawn body)
  (push-thread! (lambda (val) (body) (quit))))

; kills the current thread and removes it from the queue
(define (quit)
  (quit-k (void)))

; hand control from the current thread to another thread
(define (yield)
  (call-with-current-continuation
   (lambda (k)
     (push-thread! k)
     (quit))))

; kill all threads
(define (halt)
  (set! threads '())
  (quit))

(define vs '())
(spawn (lambda ()
         (set! vs (append vs '(1)))
         (yield)
         (set! vs (append vs '(2)))))
(spawn (lambda ()
         (set! vs (append vs '(3)))
         (yield)
         (set! vs (append vs '(4)))))
(start-threads)
vs
)

We represent the thread queue with a list of threads, where the next thread in the queue is the first in the list. When we push a thread onto the queue, we add it to the end of the list.

Like with generators, we keep track of one continuation for jumping out, @racket[quit-k], which is analogous to @racket[yield-k] from generators. But instead of just one @racket[resume-k] like with generators, we keep track of a collection of body continuations to resume to, @racket[threads].

As far as continuation stuff goes, it's very similar to how we did generators, but with multiple continuations to resume to and slightly different organization.

@; TODO elaborate and explain more

@subsection{Back Tracking}

@#reader scribble/comment-reader
(examples #:label #f #:eval eval
; queue of zero-argument functions that invoke continuations.
(define search-branches (list))

; forks for each value.
; tries the next search branch in the queue.
; Not necessarily one of the branches introduced by this call
(define (choice vals)
  (call-with-current-continuation
   (lambda (k)
     (set! search-branches (append search-branches
                                   (for/list ([val vals])
                                     (lambda () (k val)))))
     (when (empty? search-branches)
       (error "search failed"))
     (define next (first search-branches))
     (set! search-branches (rest search-branches))
     (next))))

(define (assert condition)
  (if condition
      (void)
      ; (choice (list)) kills this search branch
      (choice (list))))

(define (find-pythag)
  (define a (choice (list 1 2 3 4 5)))
  (define b (choice (list 1 2 3 4 5)))
  (define c (choice (list 1 2 3 4 5)))
  (assert (equal? (+ (* a a) (* b b))
                  (* c c)))
  (assert (<= a b))
  (list a b c))
(find-pythag)
)

@; TODO explain general
@; TODO explain for/list, when

@section{How to Implement Continuations}

One way to add support for continuations in your programming language is to translate the source program to continuation-passing style, or CPS. Here is an example of something close to CPS:

@examples[
#:label #f
#:eval eval
(define (factorial n k)
  (if (equal? n 0)
      (k 1)
      (factorial (- n 1) (lambda (fac) (k (* n fac))))))
(define (factorial-plus-one n k)
  (factorial n (lambda (fac) (k (+ 1 fac)))))
(factorial-plus-one 3 (lambda (x) x))
]

@; TODO adjust to make it more like what we compile to

In CPS, every function takes in an extra parameter, @racket[k], representing what you want to do with the answer. As you may have guessed, this @racket[k] represents a continuation. The expectation is that the function will call @racket[k] with the value that it would normally just return. For factorial, in our base case, we return @racket[(k 1)]. But in the recursive case, instead of wrapping the recursive call in a multiplication, we wrap the continuation that we pass to the recursive call in a multiplication. This is a common pattern in CPS. When we want to do something to the result of a function call, we instead do it in the continuation that we pass to the function. After all, that @racket[k] is "what we want to do with the answer". Similarly, in @racket[factorial-plus-one], we do the adding one in the continuation that we pass to the factorial function. And finally, when we actually want to get the value at top-level, we pass the identity function as continuation because we just want the answer. This is how we "leave CPS land".

There are a few interesting things about CPS. One of them is that everything is a tail call. In true CPS, even operations like addition and multiplication would take in a continuation. In fact, even constant expressions like @racket[1] take in a continuation! We'll see how this works soon when we write a translator to CPS. Anyway, in true CPS, literally everything would be a tail call. This means if you translate your programs to CPS and have tail call optimization, you don't need a runtime stack!

Now let's get into the translation. In our translation, translating an expression into CPS will result in an expression which is a function that takes in a continuation and calls the continuation with the result of evaluating the expression.

Before we dive into implementation, which will involve a lot of potentially confusing features of Racket, let's focus on the rewrite rules symbolically.

For now, an expression \(e\) is one of:

A variable reference \(x\)

A constant \(c\) like a number

A single argument lambda \(\lambda x . e\)

A function application \(e e\)

For example, the Racket expression @racket[(((lambda (x) (lambda (y) x)) 1) 2)] would be written as \(((\lambda x . \lambda y . x) 1) 2\).

The result of translating an expression will be a function expression that takes in a continuation. The translated expression will call that continuation with its value.

For a variable reference or a constant expression, the translation is simple:

\[[x] = \lambda k . k x\]

We compile the variable reference or constant expression to a lambda that takes in a continuation and immediately calls it on itself. We denote the translation with square brackets. In our toy example, we didn't bother doing this, but we should have been doing it for all variable references and numbers. Luckily, we are writing a translator that will do this for us.

Lambdas get an extra argument for the continuation, but since the lambda is an expression, we must also wrap it in a continuation function like we did for the constants:

\[
[\lambda x . e] = \lambda k . k (\lambda x . \lambda k' . [e] k')
\]

We compile to a function that takes in a continuation \(k\) and calls it on the value of the expression, which is the inner lambda. The inner lambda takes in \(x\) as before, but also takes in an additional continuation argument \(k'\) representing what the caller wants to do with the result of the body. Then, we recursively translate the body \(e\), which is going to be a lambda that takes in a continuation for what to do with its value. And that's exactly what \(k'\) is, so we pass it \(k'\).

Here is the rewrite rule for an application:

\[
[e_1 e_2] = \lambda k . [e_1] \lambda f . [e_2] \lambda x . f x k
\]

where \(e_1\) is the function and \(e_2\) is its argument.

As always, we start with \(\lambda k\), but we don't use \(k\) immediately. Instead, we want to get the values of \(e_1\) and \(e_2\) so we can call the function with the argument. Since we're in CPS, translating \(e_1\) results in a function that accepts a continuation for what you want to do with the value. So we pass it a continuation that binds the result of \(e_1\) to \(f\) so we can use it later. Next, we do the same thing for the argument \(e_2\) and bind it to \(x\). Now, since we have both values, we can actually apply the function \(f\) to \(x\). But since we're in CPS, \(f\) takes in an additional continuation for what to do with the result of the function call. That's exactly what \(k\) is for, so we pass \(k\) to \(f\).


Let's look at some examples. First, let's translate @racket[(lambda (y) y)]

\[
[\lambda y . y]
\]\[
\lambda k_1 . k_1 (\lambda y . \lambda k' . [y] k')
\]\[
\lambda k_1 . k_1 (\lambda y . \lambda k' . (\lambda k_2 . k_2 y) k')
\]

Now, let's see what happens when we apply this to a constant:

\[
[(\lambda y . y) 2]
\]\[
\lambda k_1 . [\lambda y . y] \lambda f . [2] \lambda x . f x k_1
\]\[
\lambda k_1 . (\lambda k_2 . k_2 (\lambda y . \lambda k' . (\lambda k_3 . k_3 y) k')) \lambda f . (\lambda k_4 . (k_4 2)) \lambda x . f x k_1
\]

As you can see, these expressions quickly get pretty big and hard to understand. I actually think it's easier to think of the translation in the general sense than by looking at concrete examples. Just remember the invariant that we always take in a continuation and call it with the answer. And to use the result of an expression, we call it with a continuation for what we want to do with it.

Now we're ready for @racket[call-with-current-continuation]. This doesn't need any crazy runtime implementation. It just has its own special rewrite rule that doesn't follow the normal behaviors:

\[
[\mathrm{call/cc}\ e] = \lambda k . [e] (\lambda f . f (\lambda v . \lambda k' . k v) k)
\]

\(\mathrm{call/cc}\) is short for @racket[call-with-current-continuation]. Racket also has this shorthand @racket[call/cc].

This is very subtle, so let's go through it slowly. \(k\) is the continuation for the whole \(\mathrm{call/cc}\) application. In other words, it's the current continuation! \(f\) is the value of \([e]\), which is the function we pass to @callcc . Remember, that function takes in the current continuation. But in CPS land, functions also take in a continuation as an extra argument. This is why we pass it a lambda and \(k\). The lambda that we pass to \(f\) is where the magic happens. Remember, this lambda is meant to give \(f\) access to the current continuation, \(k\). Since \(f\) is a function in CPS land, we must pass it a function that looks like a CPS'ed function, which takes in a value and a continuation for what to do with the answer. So we make a "fake" CPS'ed function that takes in the value that gets passed to \(k\) to fill in the hole, \(v\), and the continuation of the application of this lambda, \(k'\). But when \(f\) calls the \(k\), we throw away that \(k'\) continuation and use \(k\) instead. That's why we ignore \(k'\) and instead use \(k v\), applying the current continuation to the value that should fill in the hole. This ignoring of \(k'\) is why we get that aborting behavior. \(k'\) has the context of what was going on in \(f\), and we throw it away and use \(k\) instead. But if \(f\) never calls \(k\), we don't get any of this weirdness, which is why usages of @callcc that don't use @racket[k] don't abort. In this case, \(f\) will end up calling \(k\) with the result of its body since \(k\) gets passed as the second argument to \(f\). So the first argument to \(f\) is our way of providing \(f\) with \(k\) early and bypassing the normal control flow, and the second argument just causes \(f\) to behave as usual, resuming the normal control flow.

That's the essence of CPS and @callcc . We rewrite the program to work with these continuations everywhere so we can manipulate them to bypass the typical control flow with @callcc .

Now that we have the essence of the rewrite rules, we can implement them.

In racket, the syntax is very simple. Pretty much everything is either an atomic expression like a number, string, or variable, or a list of expressions surrounded by parentheses. This allows us to easily manipulate programs as data. For our translation, we will be converting regular Racket expressions into CPS Racket expressions. After our tranlation, we will simply invoke the Racket interpreter to evaluate our CPS expression.

Before we get into translation, let's talk about some Racket stuff regarding expressions as data.

In Racket, we have a data type called a symbol, which is kind of like a string. We use symbols to represent variable names. Symbols are written with a quote before a variable name like @racket['x]. @racket[gensym] is a function which generates a random symbol, and you can optionally pass in a base symbol. For example:

@examples[
#:label #f
#:eval eval
(gensym)
(gensym 'k-const)
]

We use @racket[gensym] to generate unique variable names. This will make it so we don't have to worry about those subscripts like \(k_1,k_2\) to disambiguate between different generated variables for continuations.

The quote character is not only used for creating symbols. If we prefix an expression with a quote, we get the expression itself as data instead of the result of evaluating it. For example:

@examples[
#:label #f
#:eval eval
'(+ 1 2)
(list '+ 1 2)
(equal? '(+ 1 2) (list '+ 1 2))
'1
]

The expression @racket[(+ 1 2)] is represented by a list containing the symbol @racket['+] and two numbers 1 and 2. Quoting the expression gives us this list instead of evaluating to 3.

We also see that quoting a number does nothing. This is because number expressions are just numbers.

Quoting is very useful when you're working with expressions as data. But how would we create an expression with a @racket[gensym] in it? We could just make a list like

@examples[
#:label #f
#:eval eval
(define var (gensym 'f))
var
(list var 'x 'y)
]

But if we're generating complicated expressions, having to manually use @racket[list] and quote all your variable names will be tedious and make it harder to see what expression we're generating. We have a tool called quasiquote which is perfect for this. Quasiquote uses the backtick instead of the quote character. It works similarly to quote:

@examples[
#:label #f
#:eval eval
`x
`(+ 1 2)
]

The difference is that you can escape the quotation with the comma character and have an expression evaluated inside of the quasiquote. For example:

@examples[
#:label #f
#:eval eval
(define var (gensym 'f))
var
'(var x y)
(list var 'x 'y)
`(,var x y)
]

Instead of putting the symbol @racket['var] in the list like when we used normal quotation, we evaluate @racket[var] and put that in the list.

It's like JavaScript's template literals and Python's format strings, but for expressions instead of strings.

Now that we have that out of the way, let's start with the translation for constant expressions:

@examples[
#:label #f
#:eval eval
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (cps-transform expr)
  (define k (gensym 'k-const))
  `(lambda (,k) (,k ,expr)))

(cps-transform 1)

(define (eval/cps expr)
  ((eval (cps-transform expr) ns) (lambda (x) x)))

(eval/cps 1)
]

The namespace stuff isn't important to understand for our purposes, so let's ignore it.

We have our function @racket[cps-transform], which takes in an expression and returns a CPS expression. Then, we have @racket[eval/cps] which calls our translation function and invokes the racket interpreter to evaluate our CPS expression. Then, since a CPS expression is a function which expects a continuation, we pass it the identity function to "leave CPS land" and get a value.

The built-in @racket[eval] function expects an expression as data, like what we're working with, and evaluates it to a value.

For constant expressions like numbers, we wrap them in a function that takes in a continuation and just applies it to the constant. But remember, we're returning an expression that evaluates to a function, we're not returning a function. This is a program-to-program transformation that happens before any evaluation. That's why we have to @racket[eval] the result of our translation.

Before we tackle more complicated types of expressions, we have to talk about pattern matching. This is a tool for performing case analysis based on the shape and content of data. For example:

@examples[
#:label #f
#:eval eval
(define (fib n)
  (match n
    [0 1]
    [1 1]
    [n (+ (fib (- n 1)) (fib (- n 2)))]))
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
]

Here, we are performing a case analysis on @racket[n]. @racket[0] is a pattern that matches the value @racket[0], @racket[1] matches @racket[1], and the variable pattern @racket[n] matches any value and binds it to @racket[n]. This is something you could do with a simple switch statement in popular languages like Java. But pattern matching can also be used to inspect the shape of nested data:

@examples[
#:label #f
#:eval eval
(define (get-variable-references expr)
  (match expr
    [(list 'lambda (list argument-name) body)
     (get-variable-references body)]
    [(list function-expr argument-expr)
     (append (get-variable-references function-expr)
             (get-variable-references argument-expr))]
    [variable-name (list variable-name)]))
(get-variable-references 'x)
(get-variable-references '(lambda (x) (lambda (y) x)))
(get-variable-references '((lambda (x) x) y))
]

We have three types of expressions: (single argument) lambda expressions, function applications, and variable references. And we compute a list containing all variable references. The pattern @racket[(list 'lambda (list argument-name) body)] matches a 3-element list where the first element is the symbol @racket['lambda], the second element is a singleton list containing an argument name, which we bind to the variable @racket[argument-name], and the third element is the body expression of the lambda. Since the argument name is not a variable reference, we don't include it in the output, and just recur on the body. Function applications are just a list of expressions, so we use a simple 2-element list pattern. The first element is a function expression and the second element is the argument expression. We recur on both and append the lists of variable names from both expressions. Finally, the only other type of expression is a variable reference, so we can just use a variable pattern and return the singleton list containing the variable name.

The order of cases matters. In a @racket[match], the first pattern that matches is the only one that gets used. So if we put the variable expression case first, since the pattern is just a variable pattern, it'll match anything. Then we'd be treating everything as a variable reference, which would be nonsense. We have to be careful with the order of our patterns. In general, we put more specific patterns first and more general ones last to avoid false matches.

Similar to how we have quasiquote for building expressions as data, we have quasiquote patterns for matching on expressions.

@examples[
#:label #f
#:eval eval
(define (get-variable-references expr)
  (match expr
    [`(lambda (,argument-name) ,body)
     (get-variable-references body)]
    [`(,function-expr ,argument-expr)
     (append (get-variable-references function-expr)
             (get-variable-references argument-expr))]
    [variable-name (list variable-name)]))
(get-variable-references 'x)
(get-variable-references '(lambda (x) (lambda (y) x)))
(get-variable-references '((lambda (x) x) y))
]

With quasiquote, patterns escaped by commas are matched as patterns, and things that aren't escaped are matched against their quoted values.

Now, let's translate lamdbas and function applications to CPS.

@examples[
          #:label #f
          #:eval eval
(define (cps-transform expr)
  (match expr
    [`(lambda ,args ,body)
     (define k (gensym 'k-lam))
     (define cont (gensym 'cont))
     (define body^ (cps-transform body))
     `(lambda (,k) (,k (lambda ,(append args (list cont)) (,body^ ,cont))))]
    [`(,f ,xs ...)
     (define k (gensym 'k-app))
     `(lambda (,k) ,(cps-transform-app (append (list f) xs) k))]
    [const-expr
     (define k (gensym 'k-const))
     `(lambda (,k) (,k ,const-expr))]))

(define (cps-transform-app exprs k)
  (define exprs^ (map cps-transform exprs))
  (define vs (map (lambda (_) (gensym 'v-app)) exprs))
  (foldr (lambda (expr^ v body) `(,expr^ (lambda (,v) ,body)))
         (append vs (list k))
         exprs^
         vs))

(cps-transform '(lambda (x) x))
(cps-transform '(f x y z))
(cps-transform '((lambda (x) x) 2))
(eval/cps '((lambda (x) x) 2))
]

One difference between the rewrite rules and this is that functions can take in more than one argument. We just add an argument for the continuation at the end.

For the lambda case, we capture the list of argument names and the body expression with our pattern. Then, we generate random variable names for \(k\) and \(k'\) from the rewrite rules. Finally, we build our CPS'ed lambda using quasiquote. We add an argument for the \(k'\) continuation at the end of the argument name list and we call the translated body with that continuation.

The application case looks complicated, especially now that we are dealing with an arbitrary number of arguments. Really, all we're doing is generating a chain of nested applications that give us access to the values of the function and its arguments, and then we put it all together by applying the function to its arguments and the continuation. We also wrap the whole thing with a lambda that takes in a continuation as usual. The @racket[foldr] is used to generate that nested structure from the elements of the list. It's sort of like @code{Array#reduce} in JavaScript.

Now, let's add @callcc into the mix:

@examples[
          #:label #f
          #:eval eval
(define (cps-transform expr)
  (match expr
    ['call-with-current-continuation
     '(lambda (k-cc)
        (k-cc
         (lambda (f k)
           (f (lambda (val cont) (k val))
              k))))]
    [`(lambda ,args ,body)
     (define k (gensym 'k-lam))
     (define cont (gensym 'cont))
     (define body^ (cps-transform body))
     `(lambda (,k) (,k (lambda ,(append args (list cont)) (,body^ ,cont))))]
    [`(,f ,xs ...)
     (define k (gensym 'k-app))
     `(lambda (,k) ,(cps-transform-app (append (list f) xs) k))]
    [const-expr
     (define k (gensym 'k-const))
     `(lambda (,k) (,k ,const-expr))]))
]

Instead of translating an application of @callcc like in our rewrite rules, we just translate the variable reference directly.

Here is how we express our translation as a rewrite rule:

@; [\mathrm{call/cc}\ e] = \lambda k . [e] (\lambda f . f (\lambda v . \lambda k' . k v) k)

\[
[\mathrm{call/cc}] = \lambda k_{cc} . k_{cc} (\lambda f . \lambda k . f (\lambda v . \lambda k' . k v) k)
\]

It's the same idea, there are just some things shuffled around and we don't have to inline the application rule. We are creating a CPS land function, that inner lambda, which takes in a function \(f\) and the current continuation \(k\), which comes from applying @callcc , and doing pretty much the same thing we did before with \(f\).

@racket[k-cc] is the continuation for the variable reference to @callcc , which isn't very interesting. @racket[f] is the function that receives the current continuation, @racket[k]. This is the continuation for the application of the @callcc . @racket[val] is the value that gets filled in for the hole. @racket[cont] is the continuation for the application of @racket[k], which gets ignored. That's like \(k'\) in the rewrite rule. We don't have to worry about variable collisions since @callcc gets translated to the exact same expression every time, so no need for @racket[gensym] or quasiquote.

It'll be hard to test out our @callcc with just pure lambdas, so let's add a built-in function for addition:


@examples[
          #:label #f
          #:eval eval
(define (cps-transform expr)
  (match expr
    ['call-with-current-continuation
     '(lambda (k-cc)
        (k-cc
         (lambda (f k)
           (f (lambda (val cont) (k val))
              k))))]
    ['add1
     '(lambda (k-add1) (k-add1 (lambda (n cont) (cont (add1 n)))))]
    [`(lambda ,args ,body)
     (define k (gensym 'k-lam))
     (define cont (gensym 'cont))
     (define body^ (cps-transform body))
     `(lambda (,k) (,k (lambda ,(append args (list cont)) (,body^ ,cont))))]
    [`(,f ,xs ...)
     (define k (gensym 'k-app))
     `(lambda (,k) ,(cps-transform-app (append (list f) xs) k))]
    [const-expr
     (define k (gensym 'k-const))
     `(lambda (,k) (,k ,const-expr))]))
]

We added @racket[add1], which adds 1 to a number. We wrapped it in a continuation lambda and a lambda which takes in the number and an extra continuation argument. But inside, we just call the continuation with the result of calling the real @racket[add1] function on the number. This isn't how applications usually work and breaks the fact that everything is a tail call, but that's ok.

Now, let's use @callcc :

@examples[
          #:label #f
          #:eval eval
(eval/cps '(call-with-current-continuation (lambda (k) 2)))
(eval/cps '(call-with-current-continuation (lambda (k) (k 2))))
(eval/cps '(call-with-current-continuation (lambda (k) (add1 (k 2)))))
(eval/cps '(call-with-current-continuation (lambda (k) (add1 (k (add1 (add1 2)))))))
(eval/cps '(add1 (add1 (add1 (call-with-current-continuation (lambda (k) (add1 (k 2))))))))
]

In the last two examples, we observe the aborting behavior of @racket[k]. And in the last example, we see that @callcc does indeed capture the surrounding context.



@; TODO "address this later" I was going to talk about delimited continuations, but I don't think you need them. just mention call-with-composable-continuation
@; TODO limitations. these don't work together, no nesting, hard to tell what's captured and aborts are weird. delim clearer at least.
