#lang scribble/manual

Title: Everything from call/cc
Date: 2023-10-21T20:40:48
Tags: UNLINKED, racket, continuations, tutorials, programming-languages

@require[
  scribble/example
  @for-label[racket @except-in[racket/control set]]]
@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))
@(define racket-eval (make-base-eval '(require racket)))
@(define-syntax-rule (racket-repl body ...) (examples #:eval racket-eval #:label #f body ...))
@(define dw-eval (make-base-eval '(require racket)))
@(define-syntax-rule (dw-repl body ...) (examples #:eval dw-eval #:label #f body ...))
@; abbreviations
@(define (callcc) (racket call/cc))
@(define (cwcc) (racket call-with-composable-continuation))
@(define (dw) (racket dynamic-wind))

@callcc[] is a powerful tool for implementing custom control forms operators. However, @callcc[] can be pretty unwieldy, so people tend to use delimited, composable continuations with operators like @racket[reset] and @racket[shift]. But what if I told you that these operators can be implemented using just @racket[call/cc]?

In this post, we'll implement delimited continuations, composable continuations, @racket[dynamic-wind], and parameters all from just @callcc[]. I will assume a solid familiarity with continuations and Racket. If you aren't very familiar, then feel free to check out my @link["/blog/2023/09/continuations.html"]{continuations post} to get some background. But even having read that, you sould play around with them a lot to get familiar, because this post is pretty heavy on continuation weirdness!

<!-- more -->

@table-of-contents[]

@section{Delimited and Composable Continuations}

Without delimiters, continuations created with @callcc[] capture the entire program. Also, the continuations created with @callcc[] "jump" when you call them and they never return, so you can't use the result of calling a continuation. This is why they're called non-composable continuations. These two facts make @callcc[] confusing and difficult to use, so people usually use delimited, composable continuations with operators like @racket[reset] and @racket[shift]. But these can actually be implemented in terms of @callcc[]! So if you're working in a language with just @callcc[] and want delimited, composable continuations, you can implement them yourself.

Before we get into any implementation, I need to give credit where credit is due. Most of the code in this post is based on code found on @hyperlink["https://okmij.org/ftp/continuations/implementations.html"]{Oleg Kiselyov's blog}. This post puts some of those pieces together, expands on them, and explains them in more detail.

Let's think about what delimited continuations are. When we wrap code in a @racket[reset], continuations created inside only capture up to the @racket[reset]. In other words, calling a delimited continuation returns the result of "filling the continuation's hole" and running the rest of the @racket[reset]'s body. Calling an undelimited continuation fills in the hole and runs the rest of the whole program.

Another property of continuations is composability. Non-composable continuations are like jumps. They are functions that never return, so you can't use the result. Composable continuations actually do return their results. In my continuations post, I called these aborting and non-aborting continuations respectively. This has nothing to do with whether a continuation is delimited. However, composable continuations only make sense when the continuations are delimited.

If a composable continuation captures the whole rest of the program, it also captures the part of the program that exits the process! So the composable continuation is saying "ok I'm going to run the code from this continuation and then I'll come back to you", but then while it's doing that, the world ends, so it never gets back to you.

Side note: If you use @cwcc[] from the repl, the continuations will actually return. But if you create a Racket program that just evaluates that expression, when run the program, you'll see that the continuation doesn't return. This is because the repl pretty much wraps top-level expression in a @racket[reset] and modules don't.

Our task is to limit how much is captured by a continuation and allow us to use the results of continuations.

Now, without further ado, let's get coding!

@define[delim-impl
@repl[
(define go #f)
(define pstack '())
(define (top* program-thunk)
  (set! pstack '())
  (let ([thnk (let/cc k
                (set! go k)
                (k #f))])
    (when thnk
      (let ([v (thnk)]
            [next-k (first pstack)])
        (set! pstack (rest pstack))
        (next-k v))))
  (program-thunk))
(define-syntax-rule (top body ...) (top* (lambda () body ...)))
(define (reset* thnk)
  (let/cc k
    (set! pstack (cons k pstack))
    (go thnk))); does not return
(define-syntax-rule (reset body ...) (reset* (lambda () body ...)))
(define (call-with-composable-continuation f)
  (let/cc k
    (f (lambda (v)
         (let/cc cont
           (set! pstack (cons cont pstack))
           (k v))))))
(top (list (reset (add1 (call-with-composable-continuation (lambda (k) (* 5 (k 1))))))))
]]

@delim-impl

In case you haven't seen it, @racket[(let/cc k body ...)] is equivalent to @racket[(call/cc (lamdba (k) body ...))]. It's just easier to write.

To start, we implement something like a scheduler, or trampolining. @racket[go] is a (non-composable) continuation that takes in a zero-argument function (a thunk), and ends up jumping here to the "scheduler". This effectively throws away the context of the program as far as the runtime is concerned. We'll see why we do this soon.

@racket[go] is set to a continuation that saves its argument to @racket[thnk], runs it, stores the result in @racket[v], pops the next saved continuation off the stack, and calls it with the result of the thunk. We'll see who pushes continuations to the stack soon.

Normally, we'd just put the body of @racket[top*] at the beginning of the program. But since we're going to be running code in a repl, which delimits our continuations, we need to wrap our expressions with @racket[top] to establish the context of the scheduler every time. Pretend that instead of @racket[(program-thunk)], we just had the entire rest of the program right there.

In @racket[reset], we push the current continuation onto @racket[pstack] and jump to the scheduler with the body as a thunk. Remember, the scheduler runs the body, saves the result, pops a continuation off the stack, and calls that continuation with the result. Pushing a continuation onto the stack before jumping to the scheduler ends up causing the scheduler to jump back to that continuation with the result of running the thunk.

Let's think about a simple example, @racket[(top (list (reset 1)))]. When we reach the @racket[reset], the current continuation wraps its argument in a list and exits. We push this continuation onto the stack and jump to the scheduler with the thunk @racket[(lambda () 1)]. This sets @racket[v] to @racket[1], pops that continuation from the stack, and then calls it with @racket[1], resulting in exiting with @racket['(1)]. So if there's no funny business in the @racket[reset], it's like it's not even there.

In @cwcc[], we grab the current continuation @racket[k]. But we don't supply it to @racket[f] directly. Instead, we give @racket[f] a wrapped function that grabs @racket[cont], the current continuation at the time of applying @racket[k], and push that onto the stack before jumping to @racket[k]. Since the @racket[reset] ends up at the scheduler, @racket[k] does too. @racket[k] runs the body of the reset with the argument passed to it replacing the @cwcc[], but then since we pushed @racket[cont] onto the stack, we return to the body of the @cwcc[] instead of the @racket[reset]! Don't worry if that doesn't fully make sense yet. We'll go step by step through an example in a moment.

But the way, the reason we're doing @cwcc[] instead of @racket[shift] for now because @racket[shift] is basically @racket[call-with-composable-continuation] with an abort. @cwcc[] keeps it simple and minimizes the numer of things we have to keep in our head at once.

Now let's step through that example from the code. But before we talk about how it works under our implementation, let's make sure we understand how it works in regular Racket. The continuation @racket[k] should essentially be the function @racket[add1] since that's the only thing between the @racket[reset] and the @cwcc[]. I wrapped the whole @racket[reset] in a @racket[list] to make sure that we're only capturing up to the @racket[reset]. When we call @racket[k], we should get back 2. Then, we multiply that by 5, getting 10, and return that from the @cwcc[], which adds 1 to it and wraps it in a list.

Now how does our implementation work? I'll put the code here again so we can follow it:

@delim-impl

First, we establish the scheduler with @racket[top]. Next, we call @racket[list], so when @racket[reset] is called, the current continuation wraps its argument in a list and exits. @racket[reset] pushes this continuation onto the stack and it's the only continuation in it for now. Then, it jumps to the scheduler, discarding the context. At this point, we're at that @racket[(when thnk ...)] expression. Then, we call the thunk, which is the body of the @racket[reset].

The first thing it does is call @racket[add1]. Inside the @racket[add1], the current continuation, which we'll get as @racket[k] in a moment, adds @racket[1] to its argument, sets that to @racket[v] in the scheduler, pops a continuation off the stack, and calls it with @racket[v]. We call @cwcc[], which gives us access to this continuation, but with a wrapper that pushes the current continuation at the time of calling @racket[k], @racket[cont], onto the stack. What happens when we call @racket[k]?

We push the current continuation @racket[cont] onto the stack and call @racket[k] with @racket[1]. What does @racket[k] do? It adds @racket[1], sets that to @racket[v] in the scheduler, pops @racket[cont] off the stack, and calls it with the result. Now, we're back in the body of the @cwcc[]! And we resumed with @racket[2], which was the result of calling @racket[k]. This is exactly what we want! Then, we just multiply that @racket[2] by @racket[5] and return to the @racket[reset] body, which adds @racket[1], giving us @racket[11].

Then, we return from the @racket[reset], which returns to the scheduler, which pops a continuation off the stack. What continuation is on the stack? Remember, we originally pushed the @racket[reset]'s continuation onto the stack, then pushed @racket[cont] onto the stack, then popped it to jump back to the call site of @racket[k]. At this point, the next continuation is the @racket[reset]'s, so we call it with the result of running the body of the @racket[reset], @racket[11], which wraps that result in a list and exits. And we're done!

The big idea is that @racket[reset] saves the current continuation and jumps to the scheduler, telling it to run the body. Since the body runs in the scheduler's context, continuations captured in the body will capture the end of the scheduler, which pops the next continuation off the stack and calls it, which jumps. This is how we avoid capturing the whole rest of the program, which includes the exit. By mutating the stack of continuations, we can control where we'll end up next time we return to the scheduler.

Before we call a composable continuation @racket[k], we push the current continuation @racket[cont] onto the stack. That way, we'll run @racket[k], which will end at the scheduler, which will pop and resume at @racket[cont] with the result of running the rest of the @racket[reset] body with the argument we suppled to @racket[k]. Pushing the current continuation onto the stack causes control to return to this point when we hit the end of the scheduler. That's how we "trick" non-composable continuations into "returning a value". It's really just two jumps with some weird stuff in between, but as far as the user's concerned, it just looks like the continuation returned a value!

This is all very subtle and hard to follow. There are a lot of nonlocal jumps and there is mutation happening while we're jumping all over the place. I encourage you to run through some more examples in your head and play around until you're comfortable.

Now, let's implement @racket[shift]. Like I said before, @racket[shift] is basically @cwcc[] with an abort. Once we understand how @racket[reset] and @cwcc[] work, @racket[shift] isn't too much of a leap:

@repl[
(define (shift* f)
  (call-with-composable-continuation
   (lambda (k); stack fragment
     (go
      (lambda ()
        (f k))))))
(define-syntax-rule (shift k body ...) (shift* (lambda (k) body ...)))
]

It's the same as @cwcc[], but we wrap the body in a @racket[go], which jumps to the scheduler. Unlike @racket[reset], we don't save the continuation before we jump. This has the effect of using whatever happens to be on the top of the stack, which is usually the @racket[reset]'s continuation.

For example, let's run through

@repl[
(top (list (reset (add1 (shift k 0)))))
]

We establish the scheduler and push the @racket[reset]'s continuation onto the stack, which wraps its argument in a list and exits. When we end up at the @racket[shift], we jump to the scheduler with a thunk that returns @racket[0]. So we end up passing @racket[0] to that continuation, resulting in @racket['(0)]. When we jumped to the scheduler from the @racket[shift], we discarded the context that included the @racket[add1]. The only thing that remembered that context was @racket[k], which we ignored.

So @racket[shift] ends up replacing the entire @racket[reset] with the result of the @racket[shift]'s body. And it's no different if we call @racket[k] in the @racket[shift]. @racket[k] is basically a function as far as the user is concerned. It just happens to run the body of the @racket[reset] that we aborted from.

And there we have it! Using undelimited, non-composable continuations, we implemented delimited and composable continuations.

Now what about @racket[dynamic-wind] and parameters?

@section{Dynamic Wind}

Let's say you're working with files. You have the functions @racket[(open path)] which opens a file at the path and returns a value representing the file, and @racket[(close file)] which closes the file. We're well-behaved programmers, so we will make sure that we always close our files when we're done with them.

To make sure we never forget, let's make an abstraction that closes the file for us when we're done:

@racketblock[
(define-syntax-rule
  (with-file f path body ...)
  (let ([file (open path)])
    body
    ...
    (close path)))
]

Very nice. But what if we do this?

@racketblock[
(let/cc abort
  (with-file f path
    (abort #f)))
]

We leave the body of @racket[with-file] without closing it!

Alright, that's unfortunate, but it's not a huge deal. When the process ends, the operating system cleans up after us anyway.

But here is another example:

@racketblock[
(define saved-k #f)
(with-file f path
  (let/cc k (set! saved-k k))
  ...)
(saved-k 42)
]

If we use saved-k to re-enter the body of @racket[with-file], at this point, the file is already closed! This would cause the body to fail. This would be a problem even if the continuation was composable.

In general, there are a lot of situations where you want to run code under some context that includes setup and cleanup, and continuations don't play nice with that since they cause us to jump all over the place. What we want is an operation that allows us to run some body of code where every time we enter the body, we run some setup, and every time we leave the body, we run some cleanup, regardless of whether we naturally enter/exit or if it was because of a continuation jumping in or out. This operation is called @racket[dynamic-wind].

Let's look at some examples:

@racket-repl[
(let/cc abort
  (displayln "setup")
  (abort 42)
  (displayln "cleanup"))
(let/cc abort
  (dynamic-wind
    (lambda () (displayln "setup"))
    (lambda () (abort 42))
    (lambda () (displayln "cleanup"))))
]

Without @dw[], the cleanup doesn't run when we abort. Let's look at another example:

@racket-repl[
(define saved-k #f)
(let ()
  (displayln "setup")
  (let/cc k (set! saved-k k))
  (displayln "cleanup"))
(saved-k 42)
(dynamic-wind
  (lambda () (displayln "setup"))
  (lambda () (let/cc k (set! saved-k k)))
  (lambda () (displayln "cleanup")))
(saved-k 42)
]

Without @dw[], we don't get the setup when we re-enter.

Alright, how do we implement this thing?

When I was trying to learn how to implement it, the first implementation I found kept track of the thunks that were pending execution. We keep track of a stack of pairs of thunks. Each pair has a setup and a cleanup thunk. We maintain the invariant that a pair is in the stack if and only if the setup has been run and the cleanup hasn't yet been run in the current dynamic context.

When we call a continuation, we are jumping from one dynamic context to another. When we create a continuation, we remember the continuation's dynamic context by associating it with the current value of the stack of thunk pairs at the time of creation. When we call a continuation, we compare the current value of the stack with the continuation's and run the thunks as appropriate. But as we go, we have to carefully and incrementally change the value of the stack to maintain the invariant. And we also have to be careful about which thunks to run and the order that we run them.

This implementation is very hard to understand and reason about. There is a lot of intricate book-keeping, there is global mutable state, and we're jumping all over the place because of @callcc[].

This implementation works fine with @callcc[], but it was very difficult to try to generalize it to delimited continuations. To learn more about this strategy and see my failed attempt to generalize it to delimited continuations, you can look at @hyperlink["https://github.com/quasarbright/learn-racket/blob/0c24b442b5e2b638b3c62e57445e08f4296ee9a4/callcc-to-everything-direct.rkt"]{my implementation}.

As usual, all roads lead back to Oleg Kiselyov. His post @hyperlink["https://okmij.org/ftp/continuations/implementations.html#dynamic-wind"]{Delimited continuations do dynamic-wind} has an implementation in terms of delimited continuations. This implementation is much simpler and doesn't need to be generalized to delimited continuations! This is the implementation we'll discuss in this post.

Let's start coding:

@dw-repl[
(require racket/control)
(struct yield-record [v k] #:transparent)
(define (yield v) (shift k (yield-record v k)))
]

We define a new operator called @racket[yield], which shifts a record containing the value
yielded and the continuation of the yield. Yield is sort of like an exception we can resume from. The expectation is that we are in the context of some handler that will decide what to do with the yield record.

For example, we can use @racket[yield] to implement generators:

@dw-repl[
(define (for-generator generator-thunk loop-body)
  (let loop ([res (reset (generator-thunk))])
    (match res
      [(yield-record v k)
       (loop-body v)
       (loop (k void))]
      [_ (void)])))
(for-generator
  (lambda () (yield 1) (yield 2))
  (lambda (v) (displayln v)))
]

Here, @racket[for-generator] is the "handler" for @racket[yield]. We run the generator body in a @racket[reset] and look at the result. If we yielded from the body,the result would be a yield record. In that case, we run the @racket[loop-body] with the yielded value and recur, resuming the body. If the body ended on its own, we'd reach the other branch, in which case we'd return void and stop recurring.

How should @dw[] interact with @racket[yield]? If we yield from inside of a @dw[], then we should run the cleanup since we're leaving the body. And when we resume from the yield, we should run the setup. And the yield should go right through the @dw[] to the handler outside of it. For example:

@racketblock[
(for-generator
   (lambda ()
     (dynamic-wind
       (lambda () (displayln "setup"))
       (lambda () (yield 1))
       (lambda () (displayln "cleanup")))
     (yield 2))
   (lambda (v) (displayln v)))
]

We should print @racket[setup cleanup 1 setup cleanup 2]. First, we naturally run the setup on the way in. Then The yield exits the generator body, hence the first cleanup. In the handler, we print the yielded value, @racket[1]. Then, we resume, which reenters the body, hence the second setup. Next, we naturally run the cleanup on the way out. Finally, the next yield just results in @racket[2] getting printed with no @dw[] business.

Now, we're ready to implement @dw[]:

@dw-repl[
(define (dynamic-wind setup-thunk thunk cleanup-thunk)
  (let loop ([th (lambda () (reset (thunk)))])
    (setup-thunk)
    (let ([res (th)])
      (cleanup-thunk)
      (match res
        [(yield-record v k)
         (let ([reenter (yield v)])
           (loop (lambda () (k reenter))))]
        [r r]))))
(for-generator
   (lambda ()
     (dynamic-wind
       (lambda () (displayln "setup"))
       (lambda () (yield 1))
       (lambda () (displayln "cleanup")))
     (yield 2))
   (lambda (v) (displayln v)))
]

The structure is similar to the generator. @dw[] is a handler for yields. We initialize our recursive loop with a @racket[(reset (thunk))], but we don't run it right away because we haven't run the setup yet. Instead, we wrap it in a thunk and call it @racket[th] so we can run it later. Next, we run @racket[setup-thunk]. Only after that do we run @racket[th], which runs the main thunk. Then we run the cleanup thunk, regardless of whether we finished naturally or yielded. This is because in either case, we want to run the cleanup.

Like in the generator, if we get a yield record, that means we yielded out of the main thunk. Since we want yields to go right through the @dw[] on the way out, we re-yield @racket[v] to let the outer handler handle it. It's like re-throwing an exception. But we also need the yield to go right through on the way back in, so we save the value that the outer handler resumes with to @racket[reenter] and we resume the body by calling @racket[k] with that value. But we don't resume right away. We do it in a thunk that we recur on so we end up calling the setup before we re-enter the body. And since this is in a loop, the whole thing happens for future yields until the body finishes, in which case we return the value of the body.

This is very nice, but we don't want to use @racket[yield]. We want @racket[reset] and @racket[shift]! Luckily, we can implement @racket[reset] and @racket[shift] in terms of @racket[yield] such that they play nice with @dw[]. But we use @racket[reset] and @racket[shift] in our implementation of @dw[]. To avoid confusion, let's make it clear whether we're using the built-in operators or our operators that we're making. We can do this with a qualified import:

@dw-repl[
(require (prefix-in racket: racket/control) (prefix-in racket: racket))
(define (yield v) (racket:shift k (yield-record v k)))
(define (dynamic-wind setup-thunk thunk cleanup-thunk)
  (let loop ([th (lambda () (racket:reset (thunk)))])
    (setup-thunk)
    (let ([res (th)])
      (cleanup-thunk)
      (match res
        [(yield-record v k)
         (let ([reenter (yield v)])
           (loop (lambda () (k reenter))))]
        [r r]))))
]

To start, let's think about how @racket[reset] and @racket[shift] should interact with @dw[]. For example:

@dw-repl[
(racket:reset
  (racket:dynamic-wind
    (lambda () (displayln "setup"))
    (lambda ()
      (racket:shift k (k 1))
      (displayln "after the shift"))
    (lambda () (displayln "cleanup"))))
]

Remember, @racket[shift] aborts to the @racket[reset]. That's why we get the first cleanup. And when we call the continuation inside the @racket[shift], we re-enter the body, which is why we get the second setup. After we re-enter, we print @racket["after the shift"]. Although the @racket[(k 1)] is lexically inside of the @dw[], it executes from outside of its dynamic extent because the @racket[shift] aborts before we run it. Next, we exit naturally, which runs the cleanup.

Let's look at another example:

@dw-repl[
(define saved-k #f)
(racket:reset
  (racket:dynamic-wind
    (lambda () (displayln "setup"))
    (lambda ()
      (racket:shift k (set! saved-k k))
      (displayln "after the shift"))
    (lambda () (displayln "cleanup"))))
(saved-k 2)
]

The first setup comes naturally. The first cleanup is from the @racket[shift] aborting. When we call @racket[saved-k], we get a setup from re-entering the @dw[], we print @racket["after the shift"], and we exit naturally, running the cleanup. It's really the same thing as last example, but instead of immediately resumeing, we do it in the next repl prompt. But it's interesting that the continuation "remembers" the @dw[].


Alright, how do we implement this? Let's try using the built-in @racket[reset] and @racket[shift] and see what happens:

@dw-repl[
(racket:reset
  (dynamic-wind
    (lambda () (displayln "setup"))
    (lambda ()
      (racket:shift k (k 1))
      (displayln "after the shift"))
    (lambda () (displayln "cleanup"))))
]

Notice that we used our @dw[], but racket's @racket[reset] and @racket[shift]. We're missing the inner setup and cleanup! We only get the natural ones, not the ones from jumping out and in. This is because the shift aborts right past the @dw[] to the reset, which means jumping past the code that runs the cleanup. And we resume right at the point of the shift instead of getting stopped going back through the @dw[], skipping the setup on re-entry.

@racket[yield] is the only thing that plays nice with @dw[], so we'll implement @racket[shift] using it:

@dw-repl[
(struct shift-record [f] #:transparent)
(define (shift* f)
  (yield (shift-record f)))
(define-syntax-rule (shift k body ...) (shift* (lambda (k) body ...)))
]

Similar to how we made @racket[yield] in terms of @racket[shift], we make a struct representing the @racket[shift] operation and @racket[yield] that struct.

If we're yielding, we'll need a handler. Where should the shift get handled? Well, shift aborts out the reset, so it's the perfect place to handle it! This means that shifts without a reset will just abort the program with a shift record, but that's not a big deal. Theoretically, we could wrap the entire program in a reset and everything would work fine.

@dw-repl[
(define (reset* thnk)
  (match (racket:reset (thnk))
    [(yield-record v k)
     (match v
       [(shift-record f) (reset (f k))]
       [_ (error "unhandled yield from user reset")])]
    [r r]))
(define-syntax-rule (reset body ...) (reset* (lambda () body ...)))
(reset
  (dynamic-wind
    (lambda () (displayln "setup"))
    (lambda ()
      (shift k (k 1))
      (displayln "after the shift"))
    (lambda () (displayln "cleanup"))))
]

Like all the yield handlers we've written, we run the body in a @racket[reset]. But we use the real Racket version, not the one we're making. We inherit the delimiting behavior from the real version and add the yield handling on top of it. Anyway, if the body yields a shift record, we run the shift body with @racket[k], but wrapped in a @racket[reset]. And this is our @racket[reset], not the built-in, real version. This recursive call is just like the looping that we used in our other handlers, but instead of a let loop, we just do a recursive call directly because it's easier.

If something other than a shift record got yielded, we error. We do this because users aren't going to be using @racket[yield], only @racket[reset] and @racket[shift], which yield shift records. So we assume that any other kind of yield is from a mistake in our implementation.

You might be wondering why we bothered going through yield at all. If we want shift, why not just implement @dw[] in terms of shift instead of yield?

You could try to do that, but it's much more straightforward with yield. With yield, the handler is what drives the control flow. After all, yield doesn't give the caller access to a continuation! Only the handler gets access. And @dw[] is essentially a handler, so it fits in perfectly. The flow of the yield nicely captures the essence of jumping out of and into a dynamic extent.

With @racket[shift], the body drives the control flow, and there is no handler, which makes it harder to detect and handle exit and re-entry. We'd have to manipulate the body and the continuation or something. It'd be much trickier to figure out.

@; left off here about to do cwcc and call/cc. do naive, show that they exit, and then do passthrough. careful with consistent naming and style when you paste the final implementation in.

@;TODO check consistent naming like for the setup and cleanup thunk, check that lets have square brackets, etc.
