#lang scribble/manual

Title: Pi Calculus: The Essence of Concurrent Programming
Date: 2023-10-07T15:49:11
Tags: UNLINKED, racket, programming-languages

You may have heard of the lamdba calculus. It is a system where everything is either a function, a variable, or a function call. This is the essence of functional programming and the theoretical foundation for it. Even though it is very simple, it is just as powerful as any programming language since it is Turing-complete.

The pi calculus is a similar idea, but instead of functional programming, it is the essence of concurrent programming. In this post, we will explore and implement the pi calculus.

<!-- more -->

The pi calculus is a model of concurrent computation. In the pi calculus, the core constructs are processes and channels.
A channel is conceptually a queue of values that processes can write to and read from. Let's start by defining the different types of processes:

@racket[(out chan val proc)] is a process which writes a value @racket[val] to the channel @racket[chan] and then runs the process @racket[proc]. In the pure pi calculus, the only values are channels, but we will allow ourselves to use Racket to compute values.

@racket[(in chan val->proc)] is a process which reads a value from the channel @racket[chan], calls the function @racket[val->proc] with the value from the channel, which returns a process, and then runs that process. It blocks until it reads a value.

@racket[(with-channel chan->proc)] is a process which creates a new channel, calls the function @racket[chan->proc] with the channel, which returns a process, and then runs that process.

@racket[(branch proc1 proc2)] runs the processes @racket[proc1] and @racket[proc2] concurrently.

@racket[(duplicate proc)] runs infinite copies of @racket[proc] concurrently.

@racket[noop] does nothing.

For example, the process @racket[(with-channel (lambda (chan) (branch (out chan 2 noop) (in chan (lambda (val) noop)))))]
creates a channel that we call @racket[chan] and then concurrently writes the number @racket[2] to it and reads from it. We don't do anything with the value that we read from it though.

That's pretty much it! From these few simple operations, we can express all kinds of concurrent behavior. For example, let's write a very simple server:

@racketblock[
(duplicate (in server-in-channel (lambda (request) (out (request-out-channel request) (do-server-stuff request) noop))))
]

@racket[do-server-stuff] is a function that takes in a web request and returns the server's response. It is a Racket function, nothing to do with the pi calculus. We read in the request through @racket[server-in-channel], compute our response, and send it through @racket[(request-out-channel request)] to respond. For this server, we are expecting requests to have a field that contains the channel to send the response to, that way we can have many different clients concurrently sending requests. It's sort of like continuation-passing-style. We wrap this process in a @racket[duplicate], which causes infinite copies of it to run concurrently. This means the server will be able to handle concurrent requests and, since reading from @racket[server-in-channel] blocks until there is something in the channel, we will be listening for new requests forever.
@; TODO link continuation post.
