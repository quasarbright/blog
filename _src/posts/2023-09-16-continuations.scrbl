#lang scribble/manual

Title: Continuations
Date: 2023-09-24T12:37:35
Tags: racket, continuations, tutorials, programming-languages, understand-and-implement

@require[
  scribble/example
  @for-label[racket @except-in[racket/control set] racket/generator]]
@(define eval (make-base-eval '(require racket)))

@(define callcc (racket call-with-current-continuation))

Continuations are a powerful tool that allow you to implement control flow constructs like exceptions, generators, and multi-threading, and back tracking as libraries. That's right, libraries! In a programming language that gives access to continuations, these features don't have to be baked into the implementation of the language. In this post, we will explore what continuations are, how to use them, and how to implement them in a programming language as a pre-processing step.


<!-- more -->

@table-of-contents[]


This post is written in Racket since it gives us access to continuations, but I'll explain Racket-y stuff as we go, so you don't need to know it.

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

We can think of a continuation like a hole in the program: @racket[(+ 1 (+ 1 (+ 1 ?)))]. This is the context of evaluation at the point where we are evaluating the @racket[2]. The continuation captures the "rest of the program" after filling in the hole, and in this case, we fill in the hole with 2. The continuation is like a function that fills in the hole with some value and continues the program. In this case, it is a function that adds 3 to its input. In Racket, we can access this continuation directly:

@examples[
#:label #f
#:eval eval
(+ 1 (+ 1 (+ 1 (call-with-current-continuation (lambda (k) (k 2))))))
]

Some Racket stuff: @racket[lambda] is used for creating anonymous functions, so @racket[(lambda (x) (* 5 x))] is a function that multiplies its input by 5. Functions are called like @racket[(f x y)] instead of @code{f(x,y)}. When we write @racket[(+ 1 2)], we are calling the @racket[+] function with two arguments. @racket[call-with-current-continuation] is a special built-in function that I'll explain soon, and we're calling it with one argument, our lambda function. Our lambda @racket[(lambda (k) (k 2))] takes in a function, @racket[k], and calls it with the argument @racket[2]. There are a lot of functions here, so it's easy to get confused. But it'll become more clear with some examples.

@racket[call-with-current-continuation], or @racket[call/cc] for short, is a built-in function that gives us access to the current continuation. Using it pretty much always looks like this: @racket[(call-with-current-continuation (lambda (k) do-something-with-k))], where @racket[k] is the current continuation. The continuation @racket[k] is represented as a 1-argument function, which "fills in the hole" with the argument (2 in this case) and continues evaluating the expression. The "hole" is created where we call @callcc , so the entire call to @callcc is what gets replaced when we call @racket[k] with some value.

This is cool, but it looks like we haven't really gained anything. The result is the same, so why are we making this more complicated by involving continuations? The real power comes from the fact that we can do whatever we want with @racket[k] inside of that lambda:

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

Instead of just getting the current continuation and immediately using it by "filling in the hole" with 2, we save it to the variable @racket[saved-k] first. We still get 5, but now, we have access to that saved continuation, even after the expression is done evaluating:

@examples[
#:label #f
#:eval eval
(saved-k 2)
(saved-k 6)
(saved-k 10)
]

We can continue, or "resume", the computation as many times as we want using the continuation we captured and saved. In other words, the continuation @racket[saved-k] remembers its context. Pretty cool!

Here is another example:

@examples[
#:label #f
#:eval eval
(+ 1 (+ 1 (+ 1 (* (save-it!) 2))))
]

Now, instead of just adding three to what we fill in the hole with, we're multiplying that value by two and then adding three:

@examples[
#:label #f
#:eval eval
(saved-k 5)
(saved-k 1)
]

One weird thing about continuations created with @racket[call-with-current-continuation] is that they "abort" when you use them. For example:

@examples[
#:label #f
#:eval eval
(* (saved-k 5) (saved-k 1))
]

If @racket[saved-k] was just a regular function that multiplies its input by 2 and adds 3, we'd expect to get 65. However, we get the same result as if the whole expression was @racket[(saved-k 5)]. We never compute @racket[(saved-k 7)] or that outer multiplication. When we call a continuation created with @callcc , we throw away our context. These continuations behave like a jump or a go-to. If we want to avoid this, we can use @racket[call-with-composable-continuation] instead:

@examples[
#:label #f
#:eval eval
(define (save-it!)
  (call-with-composable-continuation
    (lambda (k) ; k is the captured continuation
      (set! saved-k k)
      (k 2))))
(+ 1 (+ 1 (+ 1 (* (save-it!) 2))))
(saved-k 5)
(saved-k 1)
(* (saved-k 5) (saved-k 1))
]

Continuations created with @racket[call-with-composable-continuation] can be treated like regular old functions.

You might've noticed that we got 17 for the first expression instead of 7. Why?

Let's see what would've happened if we didn't use @racket[k] in @racket[save-it!]:

@examples[
#:label #f
#:eval eval
(define (save-it!)
  (call-with-composable-continuation
    (lambda (k) ; k is the captured continuation
      (set! saved-k k)
      2)))
(+ 1 (+ 1 (+ 1 (* (save-it!) 2))))
(saved-k 5)
(saved-k 1)
(* (saved-k 5) (saved-k 1))
]

Now we just return @racket[2] directly instead of @racket[(k 2)], and it behaves normally. When using @racket[call-with-composable-continuation], whatever gets returned by the lambda fills in the hole. Now, we are filling in the hole with 2 initially. Before, when we were calling @racket[(k 2)] in the lambda, we were calculating @racket[(k 2)], which was 7, and returning that. Then, we ended up filling the hole with 7 since that's what the lambda returned.

The same thing happens with @callcc too:

@examples[
#:label #f
#:eval eval
(define (save-it!)
  (call-with-current-continuation
    (lambda (k) ; k is the captured continuation
      (set! saved-k k)
      2)))
(+ 1 (+ 1 (+ 1 (* (save-it!) 2))))
(saved-k 5)
]

But in our original implementation of @racket[save-it!], we called @racket[k] in the lambda. So why didn't we get 17 there too? It's because @racket[k] aborted the computation, so we never actually returned from the lambda! We just "jumped" into resuming the computation, losing the context of returning from the lambda. This is really subtle.

But we didn't end up getting 17 because This would've happened with @callcc too, but since @racket[k] aborts, we only filled in the hole once. This is very subtle.

With both @callcc and @racket[call-with-composable-continuation], returning a value from the lambda fills in the hole with that value. Since continuations created with @racket[call-with-composable-continuation] don't abort, calling @racket[k] in the lambda and returning the result ends up filling in the hole, and thus, applying the continuation, twice: Once from the call to @racket[k] and once from returning from the lambda. However, since continuations created by @callcc abort, calling @racket[k] in the lambda causes the lambda to never return, and instead, the whole computation is replaced with the value of filling in the hole, so the hole only gets filled in once, and the continuation only gets applied once.

Let's recap what we know so far: @racket[call-with-current-continuation] creates a hole in an expression, a continuation given to us as the function @racket[k], that, when called with an argument, gives us the result of evaluating the expression if you replaced the hole with that argument. Calling @racket[k] also aborts evaluation of the expression you called it in with the result of resuming the continuation, unless you use @racket[call-with-composable-continuation].

If you're anything like me, this still doesn't quite make sense. What exactly does the continuation capture? What is the order of events? How does any of this even work?!

If you play around enough, you'll start to get a feel for it. But once you start to think you understand how it works and how to use it, you'll run into a really weird example that will throw everything out the window, like @racket[(call-with-current-continuation call-with-current-continuation)]. What does that do?! Even after implementing @callcc myself, I still don't fully understand it. But you can still safely make use of it by avoiding weird stuff and being careful, for the most part.

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

To make sure we understand all the moving parts, let's go through our example step by step. Here is the code again:

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

This is inspired by @hyperlink["https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/"]{continuations by example}.

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
  (set! threads (list))
  (quit))

(define vs (list))
(spawn (lambda ()
         (set! vs (append vs (list 1)))
         (yield)
         (set! vs (append vs (list 2)))))
(spawn (lambda ()
         (set! vs (append vs (list 3)))
         (yield)
         (set! vs (append vs (list 4)))))
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
  (define a (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
  (define b (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
  (define c (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
  (assert (equal? (+ (* a a) (* b b))
                  (* c c)))
  (assert (<= a b))
  (list a b c))
(find-pythag)
)

@racket[for/list] is like a list comprehension in python. We loop through @racket[vals] and create a bunch of lambdas that call the continuation @racket[k] with each value. @racket[when] is like an if statement in most languages. The body only runs when the condition is true.

This is pretty similar to multi threading, but we store zero-argument functions instead of continuations since we need those functions to resume with a particular value, namely @racket[val]. We also don't store a quit continuation.

This example is a little weird. We have @racket[find-pythag], which we think of as encapsulating this search operation. But really, it's as if the whole program is a search. In a sense, we're still in the search right now!

@examples[
#:label #f
#:eval eval
(length search-branches)
]

There are still many search branches pending, and we theoretically could jump into them if we wanted to and continue the search. It may look like the search is over, but if we made another assertion, we would backtrack:

@examples[
#:label #f
#:eval eval
(begin
  (set! search-branches (list))
  (define triple (find-pythag))
  (println triple)
  (assert (> (first triple) 3))
  (println triple))
]

There is no mutation going on here. It's just that when we first looked at @racket[triple], we were in a branch where @racket[(list 3 4 5)] was the candidate. But then, we made a new assertion which caused us to backtrack, so now it's like we're in a different timeline where @racket[triple] was @racket[(list 5 12 13)] all along. But then you'd expect us to get three prints: One for the 3 4 5 and two for the 5 12 13. I honestly can't figure out why we only get one print for the 5 12 13. Like I said, once you think you understand continuations, you'll run into some weird stuff that throws that out the window.

To limit the scope of what gets captured by these continuations and contain this weirdness, we can use delimited continuations. We will briefly explore them at the end.

@; TODO why don't we get two 5 12 13 prints? try reproducing with your impl to see if it's caching or something.
@; TODO why don't we need a quit continuation? Do we actually need one for back tracking? For generators?

@subsection{Limitations}

It's really cool that we can do this, but there are some problems with the way we've implemented these features. One is that they're too global. For example, since we only have one variable for the yield and resume continuations in the generator, we can only have one generator active at a time. Otherwise, they'll overwrite each other's saved continuations and it'll be a mess. For similar reasons, weird things happen when we try to use multithreading in two ways at once, or our backtracking search. And our back tracking search leaks into the rest of the program, as we saw. We want these things to be local, re-usable and contained. Some of this could be alleviated with clever book-keeping to avoid problems like overwriting saved continuations. But Problems like the back tracking search leaking into the rest of the program are trickier to solve. To control the scope of our "effects", we can use delimited continuations, which I'll explain at the end.

Even without these problems, continuations are still very confusing. They break local reasoning in your code since they can change the control flow and jump around, and the semantics are difficult to reason about in general. Continuations are best used to implement higher level features like generators, which are simple enough to use and reason about. Using continuations directly in your code is a recipe for confusion and weirdness.

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

There are a few interesting things about CPS. One of them is that everything is a tail call. In true CPS, even operations like addition and multiplication would take in a continuation, so we'd have @racket[(* n fac k)], which would actually be a tail call. In fact, even constant expressions like @racket[1] would take in a continuation! We'll see how this works soon when we write a translator to CPS. Anyway, in true CPS, everything would be a tail call. This means if you translate your programs to CPS and have tail call optimization, you don't need a runtime stack!

If you've ever learned about tail recursion, you've probably seen the accumulator pattern. Here is factorial written this way:

@examples[
#:label #f
#:eval eval
(define (factorial n acc)
  (if (equal? n 0)
      acc
      (factorial (- n 1) (* n acc))))
(factorial 4 1)
]

Look familiar? If you squint, these two implementations look very similar. In a sense, continuations are like the mother of all accumulators. Throughout the whole program, we're accumulating the current continuation, which is "what we want to do with the answer".

Now let's get into the translation. In our translation, translating an expression into CPS will result in an expression which is a function that takes in a continuation and calls the continuation with the result of evaluating the expression.

Before we dive into implementation, which will involve a lot of potentially confusing features of Racket, let's focus on the rewrite rules symbolically.

For now, an expression @racket[e] is one of:

A variable reference @racket[x]

A constant @racket[c] like a number

A single argument lambda @racket[(lambda (x) e)]

A function application @racket[(e1 e2)]

The result of translating an expression will be a function expression that takes in a continuation. The translated expression will call that continuation with its value.

For a variable reference or a constant expression, the translation is simple:

@racket[[x] ~> (lambda (k) (k x))]

We denote the translation of an expression @racket[e] with square brackets @racket[[e]].

We translate the variable reference or constant expression to a lambda that takes in a continuation and immediately calls it on itself. In our toy example, we didn't bother doing this, but we should have been doing it for all variable references and numbers. Luckily, we are writing a translator that will do this tedious task for us.

Lambdas get an extra argument for the continuation, but since the lambda is an expression, we must also wrap it in a continuation function like we did for the constants:

@racket[[(lambda (x) e)] ~> (lambda (k) (lambda (x cont) ([e] cont)))]

We compile to a function that takes in a continuation @racket[k] and calls it on the value of the expression, which is the inner lambda. The inner lambda takes in @racket[x] as before, but also takes in an additional continuation argument @racket[cont] representing what the caller wants to do with the result of the body. Then, we recursively translate the body @racket[e], which is going to be a lambda that takes in a continuation for what to do with its value. And that's exactly what @racket[cont] is, so we pass it @racket[cont].

Here is the rewrite rule for an application:

@racket[[(e1 e2)] ~> (lambda (k) ([e1] (lambda (f) ([e2] (lambda (x) (f x k))))))]

where @racket[e1] is the function and @racket[e2] is its argument.

As always, we start with @racket[(lambda (k) ...)], but we don't use @racket[k] immediately. Instead, we want to get the values of @racket[e1] and @racket[e2] so we can call the function with the argument. Since we're in CPS, translating @racket[e1] results in a function that accepts a continuation for what you want to do with the value. So we pass it a continuation that binds the result of @racket[e1] to @racket[f] so we can apply it later. Next, we do the same thing for the argument @racket[e2] and bind it to @racket[x]. Now, since we have both values, we can actually apply the function @racket[f] to @racket[x]. But since we're in CPS, @racket[f] takes in an additional continuation argument for what to do with the result of the function call. That's exactly what @racket[k] is for, so we pass @racket[k] to @racket[f].

Let's look at some examples. First, let's translate @racket[(lambda (y) y)]:

@racketblock[
[(lambda (y) y)]

(lambda (k1) (k1 (lambda (y cont) ([y] cont))))

(lambda (k1)
  (k1 (lambda (y cont)
        ((lambda (k2) (k2 y)) cont))))
]

Now, let's see what happens when we apply this to a constant:
@racketblock[
[((lambda (y) y) 2)]

(lambda (k1)
  ([(lambda (y) y)]
   (lambda (f)
     ([2]
      (lambda (x)
        (f x k1))))))

(lambda (k1)
  ((lambda (k2)
     (k2 (lambda (y cont)
           ((lambda (k3) (k3 y)) cont))))
   (lambda (f)
     ((lambda (k4) (k4 2))
      (lambda (x)
        (f x k1))))))
]

As you can see, these expressions quickly get pretty complicated. I actually think it's easier to think of the translation in the general sense than by looking at concrete examples. Just remember the invariant that we always take in a continuation and call it with the answer. And to use the result of an expression, we call it with a continuation for what we want to do with it.

Now we're ready for @racket[call-with-current-continuation]. This doesn't need any crazy runtime implementation. It just has its own special rewrite rule that doesn't follow the normal behaviors:

@racket[[call-with-current-continuation] ~> (lambda (k-cc) (k-cc (lambda (f k) (f (lambda (v cont) (k v)) k))))]

This is very subtle, so let's go through it slowly. @callcc is a function in CPS land, which takes in an argument, @racket[f], which is the function that takes in the current continuation, and an extra argument @racket[k], the continuation at the point of the application of @callcc . @racket[k] is the continuation for the whole @callcc application. In other words, it's the current continuation! The lambda that we pass to @racket[f] is where the magic happens. Remember, this lambda is meant to give @racket[f] access to the current continuation, @racket[k]. Since @racket[f] is a function in CPS land, we must pass it a function that looks like a CPS'ed function, which takes in a value and a continuation for what to do with the answer. So we make a "fake" CPS'ed function that takes in the value @racket[v] that gets passed to @racket[k] to fill in the hole, and the continuation of the application of this lambda, @racket[cont]. But when @racket[f] calls the @racket[k], we ignore that @racket[cont] continuation and use @racket[k] instead. This ignoring of @racket[cont] is why we get that aborting behavior. @racket[cont] has the context of what was going on in @racket[f], and we throw it away and use @racket[k] instead. But if @racket[f] never calls @racket[k], we don't get any of this weirdness, which is why usages of @callcc that don't use @racket[k] don't abort. In this case, @racket[f] will end up calling @racket[k] with the result of its body since @racket[k] gets passed as the second argument to @racket[f]. Remember, @racket[f] is a CPS'ed function, so its second argument is a continuation that it calls with its result. So the first argument to @racket[f] is our way of providing @racket[f] with @racket[k] early and bypassing the normal control flow, and the second argument just causes @racket[f] to behave as usual, resuming the normal control flow.

That's the essence of CPS and @callcc . We rewrite the program to work with these continuations everywhere so we can manipulate them to bypass the typical control flow with @callcc .

Here is the rewrite for @racket[call-with-composable-continuation]:

@racket[[call-with-composable-continuation] ~> (lambda (k-cc) (k-cc (lambda (f k) (f (lambda (v cont) (cont (k v))) k))))]

The only difference is that instead of ignoring @racket[cont], the continuation of applying @racket[k] in @racket[f], we call it with the result of @racket[(k v)]. This means calling @racket[k] actually returns a result that we can use in @racket[f]. Remember, the abort behavior of @callcc was caused by us ignoring @racket[cont]. This is because @racket[cont] contains the context of where we called @racket[k] in @racket[f]. But now that we use it, we aren't aborting anymore.

But @racket[(cont (k v))] isn't a tail call! Thus, the power we gain with @racket[call-with-composable-continuation] comes at the cost of space.

Now that we have the essence of the rewrite rules, we can implement them.

In racket, the syntax is very simple. Pretty much everything is either an atomic expression like a number, string, or variable, or a list of expressions surrounded by parentheses. This allows us to easily manipulate programs as data. For our translation, we will be converting regular Racket expressions into CPS Racket expressions. After our tranlation, we will simply invoke the Racket interpreter to evaluate our CPS expression.

Before we get into translation, let's talk about some Racket stuff regarding expressions as data.

In Racket, we have a data type called a symbol, which is kind of like a string. We use symbols to represent variable names. Symbols are written with a quote before a variable name like @racket['x]. @racket[gensym] is a function which generates a unique symbol, and you can optionally pass in a base symbol. For example:

@examples[
#:label #f
#:eval eval
(gensym)
(gensym 'k-const)
]

We use @racket[gensym] to generate unique variable names. This will make it so we don't have to worry about conflicting variable names for continuations.

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

It's like JavaScript's template literals and Python's format strings, but for building expressions instead of strings.

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

The ellipsis in the pattern for the function application case just binds the rest of the list after @racket[f] to @racket[xs].

One difference between the rewrite rules and this is that functions can take in more than one argument. We just add an argument for the continuation at the end.

For the lambda case, we capture the list of argument names and the body expression with our pattern. Then, we generate random variable names for @racket[k] and @racket[cont] from the rewrite rules. Finally, we build our CPS'ed lambda using quasiquote. We add an argument for the @racket[cont] continuation at the end of the argument name list and we call the translated body with that continuation.

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

@racket[k-cc] is the continuation for the variable reference to @callcc , which isn't very interesting. @racket[f] is the function that receives the current continuation, @racket[k]. This is the continuation for the application of the @callcc . @racket[val] is the value that gets filled in for the hole. @racket[cont] is the continuation for the application of @racket[k], which gets ignored. We don't have to worry about variable collisions since @callcc gets translated to the exact same expression every time, so no need for @racket[gensym] or quasiquote.

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
(eval/cps '(call-with-current-continuation (lambda (k) 0)))
(eval/cps '(call-with-current-continuation (lambda (k) (k 0))))
(eval/cps '(call-with-current-continuation (lambda (k) (add1 (k 0)))))
(eval/cps '(call-with-current-continuation (lambda (k) (add1 (k (add1 (add1 0)))))))
(eval/cps '(add1 (add1 (add1 (call-with-current-continuation (lambda (k) (add1 (k 0))))))))
]

In the last few examples, we observe the aborting behavior of @racket[k]. And in the last example, we see that @callcc does indeed capture the surrounding context.

Let's also add support for @racket[call-with-composable-continuation]:

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
    ['call-with-composable-continuation
     '(lambda (k-cc)
        (k-cc
         (lambda (f k)
           (f (lambda (val cont) (cont (k val)))
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
(eval/cps '(add1 (call-with-current-continuation (lambda (k) (k 0)))))
(eval/cps '(add1 (call-with-current-continuation (lambda (k) (k (k 0))))))
(eval/cps '(add1 (add1 (add1 (call-with-current-continuation (lambda (k) (add1 (k 0))))))))
(eval/cps '(add1 (call-with-composable-continuation (lambda (k) (k 0)))))
(eval/cps '(add1 (call-with-composable-continuation (lambda (k) (k (k 0))))))
(eval/cps '(add1 (add1 (add1 (call-with-composable-continuation (lambda (k) (add1 (k 0))))))))
]

@section{Delimited continuations}

One weird thing about @callcc is that it essentially captures the context of the whole program. This can cause strange behavior to leak out farther than may be intended, like with our back tracking example. To limit the scope of these strange effects, we can use delimited continuations. For example:

@examples[
#:label #f
#:eval eval
(require racket/control)
(reset (add1 (shift k (k 0))))
(reset (add1 (shift k (k (k 0)))))
(add1 (reset (add1 (shift k (k (k 0))))))
(reset (add1 (add1 (shift k (k (k 0))))))
(reset (add1 (shift k 0)))
(add1 (reset (add1 (shift k 0))))
]

@racket[shift] is like @racket[call-with-composable-continuation], except instead of taking in a function for what to do with the continuation, it binds the continuation at the point of the @racket[shift] to a variable @racket[k] and then lets you use it. @racket[reset] is the delimiter for the continuations captured by @racket[shift]. Instead of capturing the entire rest of the program, @racket[k] only captures the continuation from the @racket[reset] to the @racket[shift]. In the third example, @racket[k] just captures the inner @racket[add1] since it's in the @racket[reset], so it only adds 1. But in the next example, when we move the outer @racket[add1] inside of the @racket[reset], so @racket[k] adds 2.

In the last two examples, we also see that there is some abort behavior with @racket[shift]. When we use @racket[shift] in a @racket[reset], the entire @racket[reset] is replaced with the body of the @racket[shift]. But inside of the @racket[shift], we can use @racket[k] to fill in the hole at the @racket[shift]. The continuation doesn't abort like @callcc continuations do, but @racket[shift] itself does abort.

Of course, you can also save continuations using delimited continuations:

@examples[
#:label #f
#:eval eval
(reset (add1 (shift k (set! saved-k k) 0)))
(saved-k 0)
]

Let's implement back tracking using delimited continuations:

@examples[
#:label #f
#:eval eval
(define search-branches (list))
(define quit-k #f)
(define (with-backtracking body)
  (set! search-branches (list body))
  (define result
    (reset (shift k (set! quit-k k) (k (void)))
           (when (empty? search-branches)
             (error "search failed"))
           (define next (first search-branches))
           (set! search-branches (rest search-branches))
           (next)))
  (set! search-branches (list))
  (set! quit-k #f)
  result)
(define (choice vals)
  (when (not quit-k)
    (error "cannot branch outside of a search"))
  (shift k
    (set! search-branches (append search-branches
                                  (for/list ([val vals])
                                    (lambda () (k val)))))
    (quit-k (void)))
  )
(define (assert condition)
  (if condition
      (void)
      (choice (list))))
(define (find-pythag)
  (with-backtracking
   (lambda ()
     (define a (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
     (define b (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
     (define c (choice (list 1 2 3 4 5 6 7 8 9 10 11 12 13)))
     (assert (equal? (+ (* a a) (* b b))
                     (* c c)))
     (assert (<= a b))
     (list a b c))))
(find-pythag)
(eval:error (choice (list 1 2)))
]

Now that we're using @racket[reset], we can be confident that the context captured in our continuations doesn't extend outside of the @racket[with-backtracking]. We also added some cleanup after the body runs. We could've done that cleanup in our @callcc implementation, but the continuations still would've captured more than we wanted.

Here are the rewrite rules for @racket[reset] and @racket[shift], taken from @hyperlink["https://www.deinprogramm.de/sperber/papers/shift-reset-direct.pdf"]{this paper}:

@racket[[(reset e)] ~> (lambda (k) (k ([e] (lambda (x) x))))]

We pass the identity function to the body as its continuation. Remember, this is how we leave CPS land and get a value directly. Then, we call @racket[k] on the result of doing that. This is why continuations in the body only capture up to the @racket[reset]. The body knows nothing about @racket[k], which has the context surrounding the @racket[reset]. It's like we're running the body in a sandbox.

@racketblock[
[(shift c e)] ~> (lambda (k) ((let ([c (lambda (v cont) (cont (k v)))])
                                [e])
                              (lambda (x) x)))
]

In racket, @racket[let] is used to make local variables. For example:

@examples[
#:label #f
#:eval eval
(let ([x 3]
      [y (+ 1 1)])
  (* x y))
]

For reference, here is the rewrite for @racket[call-with-composable-continuation] again:

@racket[[call-with-composable-continuation] ~> (lambda (k-cc) (k-cc (lambda (f k) (f (lambda (v cont) (cont (k v))) k))))]

We create that same @racket[(lambda (v) (cont (k v)))] and supply it to the body, but instead of the body being a function and us calling it with the lambda, we are creating a local variable for the lambda. Since we don't ignore @racket[cont], the continuations are non-aborting. However, take a close look at the continuation that we pass to the body. It's the identity function! Supplying the identity function as the continuation is how we leave CPS land and get an immediate value. Since we don't pass @racket[k] to the body and use the identity function instead, we abort with the value of the body of the @racket[shift]. And since we have the sandboxing of @racket[reset], we only abort to the @racket[reset]. Without the @racket[reset], we'd abort the whole computation.

Let's add them to our translation:

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
    ['call-with-composable-continuation
     '(lambda (k-cc)
        (k-cc
         (lambda (f k)
           (f (lambda (val cont) (cont (k val)))
              k))))]
    [`(reset ,expr)
     (define k (gensym 'k-reset))
     (define expr^ (cps-transform expr))
     `(lambda (,k)
        (,k (,expr^ (lambda (x) x))))]
    [`(shift ,c ,expr)
     (define k (gensym 'k-shift))
     (define expr^ (cps-transform expr))
     `(lambda (,k) ((let ([,c (lambda (val cont) (cont (,k val)))])
                      ,expr^)
                    (lambda (x) x)))]
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
(eval/cps '(reset (add1 (shift k 0))))
(eval/cps '(reset (add1 (shift k (k 0)))))
(eval/cps '(reset (add1 (shift k (k (k 0))))))
(eval/cps '(add1 (reset (add1 (shift k (k (k 0)))))))
(eval/cps '(reset (add1 (add1 (shift k (k (k 0)))))))
(eval/cps '(add1 (shift k 0)))
]

In the last example, we see the aborting behavior of shift when there isn't a surrounding reset. It aborts the whole computation.

Interestingly, our sandboxing in @racket[reset] also affects our non-delimited continuation operators:

@examples[
#:label #f
#:eval eval
(eval/cps '(add1 (call-with-composable-continuation (lambda (k) (k (k 0))))))
(eval/cps '(add1 (reset (add1 (call-with-composable-continuation (lambda (k) (k (k 0))))))))
]

Only the inner @racket[add1] is captured.

We can use our old operators to create delimited continuations as long as they are used inside of a @racket[reset]. @racket[reset] is what delimits the continuations, @racket[shift] is just another operator like @callcc . We don't really need @racket[shift] to do delimited continuations, but it useful to have the option to abort to the @racket[reset] and still have non-aborting continuations.

@section{Conclusion}

Continuations are a confusing, but very powerful feature for a programming language to have. They allow users of the language to create control flow constructs like generators that most languages need to bake into their implementation. They're hard to reason about, especially in certain weird situations, but we can use them to create useful tools that are simple enough to reason about. They're not something you'll end up using directly very often, but having them in a language allows people to make very powerful tools with them.

@; TODO fix v vs. val in rewrite and code
