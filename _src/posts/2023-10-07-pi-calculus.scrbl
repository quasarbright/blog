#lang scribble/manual

Title: Pi Calculus: Understanding and Implementing Concurrency
Date: 2023-10-13T05:44:53
Tags: racket, programming-languages, tutorials

@require[
  scribble/example
  @for-label[racket]]
@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

You may have heard of the lamdba calculus. It is a model of computation where everything is either a function, a variable, or a function call. It is the essence of functional programming and the theoretical foundation for modern functional programming languages. Even though it is very simple, it is just as powerful as any programming language since it is Turing-complete.

The pi calculus is a similar idea, but instead of functional programming, it is the essence of concurrent programming. For our purposes, it will serve as a simple example of a programming language with concurrency. In this post, we will explore and implement the pi calculus in Racket. This will give an idea of how modern programming languages implement concurrency.

This post requires some familiarity with Racket or any Lisp-like language. If you have read some of my Racket posts which explain Racket stuff, you should be fine. If you see something you don't understand in the code, you can click on it and the link will take you to its documentation.

<!-- more -->

The pi calculus is a model of concurrent computation. In the pi calculus, the core constructs are processes and channels. A process is a part of a running program and multiple processes can run concurrently. A channel is conceptually a queue of values that processes can write to and read from. Processes use channels to communicate with each other. Let's start by defining the different types of processes. We will embed the pi calculus within Racket, so our representation of processes will use Racket features like structs and lambdas:

@racket[(out chan val proc)] is a process which writes a value @racket[val] to the channel @racket[chan] and then runs the process @racket[proc]. In the pure pi calculus, the only values are channels, but we will allow ourselves to use arbitrary Racket values. This is not a blocking operation.

@racket[(in chan val->proc)] is a process which reads a value from the channel @racket[chan], calls the function @racket[val->proc] with the value from the channel, which returns a process, and then runs that process. It blocks until it reads a value.

@racket[(with-channel chan->proc)] is a process which creates a new channel, calls the function @racket[chan->proc] with the channel, which returns a process, and then runs that process. This is how processes get channels.

@racket[(branch proc1 proc2)] runs the processes @racket[proc1] and @racket[proc2] concurrently.

@racket[(duplicate proc)] runs infinite copies of @racket[proc] concurrently.

@racket[(noop)] does nothing.

Processes created with @racket[out] and @racket[in] have a sequential nature to them. The child process runs after the write/read. @racket[branch] and @racket[duplicate] are where we get our concurrency.

For example, the process

@racketblock[
(with-channel
  (lambda (chan)
    (branch (out chan 2 (noop))
            (in chan (lambda (val) (noop))))))
]

creates a channel that we call @racket[chan] and then concurrently writes the number @racket[2] to it and reads from it. We don't do anything with the value that we read from it though.

That's pretty much it! From these few simple operations, we can express all kinds of concurrent behavior. For example, let's write a very simple server for an API that adds 1 to the number in its request:

@racketblock[
(struct request [response-channel body] #:transparent)
(duplicate (in server-request-channel
               (lambda (request)
                 (out (request-response-channel request)
                      (add1 (request-body request))
                      (noop)))))
]

We read in the request through @racket[server-request-channel], which our clients send requests to, compute our response, and send it through @racket[(request-response-channel request)] to respond. For this server, we are expecting requests to have a @racket[response-channel] field that contains the channel to send the response to. Having just one output channel for the server wouldn't work because a client reading from it might get someone else's response since processes run in an arbitrary order.

Side note: Passing the response channel is sort of like @link["/blog/2023/09/continuations.html"]{continuation-passing style} since the output channel sent in the request is like a continuation. Continuations are very useful!

We wrap this process in a @racket[duplicate], which causes infinite copies of it to run concurrently. This means the server will be able to handle concurrent requests and, since reading from @racket[server-request-channel] blocks until there is something in the channel, we will be listening for new requests forever.

This is a very simple server where the business logic is some pure Racket function. If the server was more complex and needed some internal concurrency for its business logic, instead of a simple @racket[out] process, we'd have some more complicated process which ends up sending a response through the response channel at some point.

To complete this example, let's see what a client would look like:

@racketblock[
(with-channel
  (lambda (response-channel)
    (out server-request-channel
         (request response-channel 2)
         (in response-channel
             (lambda (response)
               ...do-something-with-response...)))))
]

We create a channel @racket[response-channel] that the server will send its response to, we send our request to the server over @racket[server-request-channel] which includes @racket[response-channel] and the body of our request, which is the value 2, we read from the response channel to wait for the server's response, and then we execute @racket[...do-something-with-response...], which is a process that uses the response somehow.

To kick this all off, we'd have some parent process that creates @racket[server-request-channel] using @racket[with-channel] and uses @racket[branch] to concurrently run our server and some clients.

Although it is pretty low-level, the pi calculus is very powerful and expressive. In fact, it is Turing-complete! To convince yourself, think about how our server example is like a function and our clients call the function by sending requests and expecting responses. Function values are represented by their input channels. With that, we have something that looks a lot like the lambda calculus. In fact, it is pretty straightforward to translate the lambda calculus to the pi calculus using this correspondence and a CPS-like transformation! And since we are using CPS, we even get @racket[call/cc]! @hyperlink["https://github.com/quasarbright/blog/blob/e4fc75b114a663d95ba560dc07924c1ca9a839a6/_src/posts/pi-calculus.rkt#L205"]{Here is an implementation}.

This is all very cool to think about, but how do we actually implement it?

Although the pi calculus is all about concurrency, we can implement the pi calculus without using any concurrency in the host language. Instead, we'll do it ourselves by making a scheduler. We will keep track of a queue of processes that are running concurrently. At each step, we will pop a process from the queue, do a little bit of work like write to or read from a channel, and then push any resulting child processes onto the queue. For example, to run a step of an @racket[(out chan val proc)] process, we write the @racket[val] to @racket[chan] and then we push @racket[proc] onto the process queue.

To start, let's define data representations for our processes:

@repl[
(struct out [chan val proc] #:transparent)
(struct in [chan val->proc] #:transparent)
(struct with-channel [chan->proc] #:transparent)
(struct branch [proc1 proc2] #:transparent)
(struct duplicate [proc] #:transparent)
(struct noop [] #:transparent)
]

Nothing surprising here, we're just making structs to represent our process types. One thing to keep in mind is that Racket evaluates eagerly, so when constructing an @racket[out] process, we evaluate the output value immediately, not necessarily when the process is running. We also evaluate the child process immediately. This shouldn't really matter, but it's something to keep in mind. Anyway, those examples that we wrote before can actually be constructed now!

@repl[
(define simple-in-out-process
  (with-channel
    (lambda (chan)
      (branch (out chan 2 (noop))
              (in chan (lambda (val) (noop)))))))
simple-in-out-process
]

These processes are just pure data right now. They won't run until we pass them to the interpreter that we're about to make.

Let's also write data definitions for channels and process queues:


@#reader scribble/comment-reader
(repl
; A Channel is a
(struct channel [[values #:mutable]] #:transparent)
; Where values is a (listof Any) and the first element is the oldest.
; Represents a queue of values that can be read from and written to from processes.
;
; A ProcessQueue is a (listof Process)
; Where the first process is the oldest.
; Represents processes running concurrently.
)

We will represent our various queue types with lists. A channel has a mutable field storing the list of values from oldest to newest. We'll push new values onto the end of the list and pop from the beginning. Process queues are similar, but they're immutable. Since there is only one process queue, we'll use a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{parameter} for the current process queue and mutate that.

@; TODO make process queue just store a mutable list, no need to use a parameter.

Now before we get to that interpreter, let's think about blocking and deadlock: When we read from a channel with an @racket[in] process, what if the channel is empty? We have to wait for there to be something in the channel before we can run the process. This means the process is blocked. When the next process we want to run is blocked, we'll skip over it for now and come back to it later. After all, one of the other processes in the queue might end up writing to its channel and unblocking it!

But we're not safe yet. What if all the processes are blocked? If we assume that nothing can write to channels other than the processes in our queue, then they'll stay blocked forever because there are no processes that can run to put anything in a channel that might unblock a process. This is called a deadlock and we'll just stop execution in this case.

For example, let's create the classic deadlock scenario of two processes waiting for each other. Alice and Bob got into an argument. They have calmed down, but they're stubborn. They're ready to apologize to each other, but they won't apologize until the other apologizes first.

@repl[
(define classic-deadlock-process
  (with-channel
    (lambda (chan-alice)
      (with-channel
        (lambda (chan-bob)
          (branch (in chan-alice (lambda (apology-from-bob) (out chan-bob "I'm sorry too, Bob" (noop))))
                  (in chan-bob (lambda (apology-from-alice) (out chan-alice "I'm sorry too, Alice" (noop))))))))))
]

Once one gets an apology, they'll apologize to the other. But since nobody goes first, neither will ever apoligize!

An even simpler example of deadlock is a single process waiting for a message that'll never come:

@repl[
(define simple-deadlock-process
  (with-channel
    (lambda (chan)
      (in chan (lambda (val) (noop))))))
]

Nobody will ever send anything to @racket[chan] because the @racket[in] process is the only one that has access to it. This poor process will wait forever, all alone.

Now that we understand the heart-breaking nature of blocking and deadlock, we're ready to implement our interpreter. We'll write it top-down:

@repl[
(define current-output-channel (make-parameter #f))
(define current-process-queue (make-parameter #f))
]

We define a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{parameter} for a special channel that will act like standard out and another parameter for the current process queue. If we didn't have something like a special output channel, we'd have no way of knowing what happened when we ran the process!

@#reader scribble/comment-reader
(repl
; Process -> (list symbol? (listof Any))
; run the process until it and its children all terminate or are all blocked
(define (run proc [num-steps #f])
  (define output-channel (new-channel))
  (parameterize ([current-output-channel output-channel]
                 [current-process-queue (list proc)])
    (define result-type
      (let loop ([num-steps num-steps])
        (cond
          [(and num-steps (zero? num-steps))
           'timeout]
          [(empty? (current-process-queue))
           'success]
          [(process-queue-all-blocked? (current-process-queue))
           'deadlock]
          [else
           (step!)
           (loop (and num-steps (sub1 num-steps)))])))
    (list result-type (channel-values (current-output-channel)))))
)

This is the entry point for our interpreter. We pretty much just loop the @racket[step!] function until we are done or reach a deadlock. At the end, we return a symbol describing how the process ended and the contents of the output channel, which is like seeing the exit code and what got printed in the console when you run a program from the command line. We optionally accept a maximum number of steps to execute so we can test the behavior of @racket[duplicate], which will always either deadlock or run forever.

Now let's write @racket[step!]:

@#reader scribble/comment-reader
(repl
; run one step of computation.
; do a little bit of work in the next unblocked process and push child processes
; onto the queue.
; assumes the program is not blocked.
(define (step!)
  (match (pop-unblocked-process!)
    [(noop)
     (void)]
    [(out chan val proc)
     (channel-push! chan val)
     (push-process! proc)]
    [(in chan val->proc)
     (push-process! (val->proc (channel-pop! chan)))]
    [(with-channel chan->proc)
     (push-process! (chan->proc (new-channel)))]
    [(branch proc1 proc2)
     (push-process! proc1)
     (push-process! proc2)]
    [(duplicate proc)
     (push-process! proc)
     (push-process! (duplicate proc))]))
)

The first thing we do is pop the next unblocked process from the queue. Then, we run a step of that process.

Let's go case by case.

@racket[noop] is unsurprisingly boring.

For @racket[out], we write to the channel and then push the child process onto the queue.

For @racket[in], we pop a value from the channel, which must exist because we are unblocked, pass the value to @racket[val->proc], which creates a new process, and then we push that process onto the queue.

For @racket[with-channel], we create a new channel, pass it to @racket[chan->proc], and push the resulting process onto the queue.

For @racket[branch], we just push both processes onto the queue.

For @racket[duplicate], we push the child process and another @racket[duplicate] process. This will cause another copy of @racket[proc] to get pushed onto the queue every time we encounter the @racket[duplicate] process. This strategy relies on the fact that we're using a queue. If we were using a stack of processes, we'd just keep running @racket[proc] over and over without getting to any of the other processes, unless it's blocked.

@racket[(duplicate proc)] has the same behavior as @racket[(branch proc (duplicate proc))], which is then the same as
@racketblock[(branch proc (branch proc (branch proc ...)))]
if we expand out that equivalence. This is another way of understanding how @racket[duplicate] creates infinite copies of @racket[proc] running concurrently.

Now, let's implement those helper functions that we used, starting with those for process queues:


@#reader scribble/comment-reader
(repl
; {ProcessQueue} -> Boolean
; are all processes definitely blocked?
(define (process-queue-all-blocked? [pq (current-process-queue)])
  (for/and ([proc pq])
    (process-blocked? proc)))

; Process -> Boolean
; is the process definitely blocked?
(define (process-blocked? proc)
  (match proc
    [(in chan _) (channel-empty? chan)]
    [(branch proc1 proc2) (and (process-blocked? proc1) (process-blocked? proc2))]
    [(duplicate p) (process-blocked? p)]
    [_ #f]))

; -> Process
; pops the next unblocked process from the current queue in place.
(define (pop-unblocked-process!)
  (define-values (proc pq) (pop-unblocked-process (current-process-queue)))
  (current-process-queue pq)
  proc)

; ProcessQueue {(listof Process)} -> (values Process ProcessQueue)
; pops the next unblocked process from the queue immutably.
; skipped-rev is an accumulator storing the blocked processes
; at the head of the queue in reverse order.
(define (pop-unblocked-process pq [skipped-rev '()])
  (cond
    [(null? pq) (error 'pop-unblocked-process "cannot pop from empty process queue")]
    [(process-blocked? (first pq)) (pop-unblocked-process (rest pq) (cons (first pq) skipped-rev))]
    [else (values (first pq) (append (reverse skipped-rev) (rest pq)))]))

; Process -> Void
; push a process onto the current queue
(define (push-process! proc)
  (current-process-queue (append (current-process-queue) (list proc))))
)

We consider a process blocked if it is obvious that it is definitely blocked. For an @racket[in], it is blocked if its channel is empty. Branches and duplications are blocked if their child processes are blocked. This is an optimization that will not affect the behavior of terminating, non-deadlocking programs. If we didn't do this, a duplicated blocked process would just run forever doing nothing instead of being recognized as a deadlock. We can't peek inside of the child process of @racket[with-channel], which is unfortunate, but that's not a big deal. Since we're not sure whether the child process will be blocked, we consider it unblocked, since we only want to declare deadlock when we're totally sure. However, this means that

@racketblock[
(duplicate (with-channel
             (lambda (chan)
               (in chan (lambda (val) (noop))))))
]

will loop forever, creating infinte lonely processes that will never receive the messages they're so patiently waiting for. Tragic.

The implementation of @racket[pop-unblocked-process] is a little complicated, but the idea is simple. We find the next unblocked process from the queue, take it out of the queue, and return it.

Remember, the queue is represented by a list where first element is the oldest. So we pop from the beginning and push onto the end.

Now let's implement the functions on channels:

@#reader scribble/comment-reader
(repl
; -> Channel
; create an empty channel
(define (new-channel) (channel '()))

; Channel -> Boolean
; is the channel empty?
(define (channel-empty? chan)
  (empty? (channel-values chan)))

; Channel Any -> Void
; push a value into the channel
(define (channel-push! chan val)
  (set-channel-values! chan (append (channel-values chan) (list val))))

; Channel -> Any
; pop the next value from the channel
(define (channel-pop! chan)
  (define val (first (channel-values chan)))
  (set-channel-values! chan (rest (channel-values chan)))
  val)
)

Nothing too surprising or interesting here, just simple queue operations that mutate a struct with a list field.

And with that, we're done! We just implemented a concurrent programming system out of a few simple primitives and some book-keeping with queues. Let's run some programs:

@repl[
(define simple-in-out-process
    (with-channel
      (lambda (chan)
        (branch (out chan 2 (noop))
                (in chan (lambda (val) (out (current-output-channel) val (noop))))))))
(run simple-in-out-process)

(struct request [response-channel body] #:transparent)
(define single-round-of-server-process
    (with-channel
      (lambda (server-request-channel)
        (branch (in server-request-channel
                    (lambda (request)
                      (out (request-response-channel request)
                           (add1 (request-body request))
                           (noop))))
                (with-channel
                  (lambda (response-channel)
                    (out server-request-channel
                         (request response-channel 2)
                         (in response-channel
                             (lambda (response)
                               (out (current-output-channel)
                                    response
                                    (noop)))))))))))
(run single-round-of-server-process)
(run classic-deadlock-process)
(run simple-deadlock-process)
(define nats-process
  (with-channel
    (lambda (chan)
      (branch (out chan 0 (noop))
              (duplicate (in chan (lambda (val)
                                    (out (current-output-channel)
                                         val
                                         (out chan (add1 val) (noop))))))))))
(run nats-process 100)
]

There are many different directions we could take this in if we wanted to continue:

@itemize[
@item{We could use macros and/or continuations to allow us to write processes in a flat way instead of having to nest everything.}
@item{We could add special channels that are hooked up to real IO like standard out, files, or the network. This would break some of the assumptions in our scheduler like the fact that only the processes in the queue can write to and read from channels, but it would enable us to write real web servers.}
@item{We could make output processes block until their message is read to avoid needing to store values in channels. This would make the scheduler a little more complicated, but it would save memory.}
@item{We could make a completely new language that compiles to the pi calculus. If we have more control over the language, we could perform more optimizations, like detecting if a @racket[with-channel]'s child process is blocked.}
@item{We could compile to the pi calculus from other concurrency systems like actors.}
]

The pi calculus is pretty cool, but I am a little disappointed that there is no pi.
