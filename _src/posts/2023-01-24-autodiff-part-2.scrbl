#lang scribble/manual

Title: Extending Automatic Differentiation to Higher Order Derivatives
Date: 2023-01-24T12:33:39
Tags: DRAFT, racket, math, machine-learning, projects, tutorials

@require[
  scribble/example
  @for-label[
    racket
  ]
]
@(define derivative (racket derivative))
@(define DNumber (racket DNumber))

This is part 2 of a series of blog posts about implementing automatic differentiation. You can read part 1 @link["/blog/2022/12/understanding-and-implementing-automatic-differentiation.html"]{here}. In this post, we extend our automatic differentiation system to support higher order derivatives.

Like the previous post, some knowledge of calculus is required and Racket-y stuff will be explained as we go.

<!-- more -->

@table-of-contents[]

In part 1, we implemented first order automatic differentiation. Our derivative function took in a @DNumber representing the output and a @DNumber representing the input and returned a plain number representing the derivative of the output with respect to the input. Recall that a @DNumber represents the result of a differentiable computation and stores a mapping from each input to the numerical value of its derivative.

@section{Higher Order Derivatives}

At the end of part 1, I teased higher order derivatives. What are higher order derivatives? In part one, we only took the first derivatives. In math, we can compute the second derivative by taking the first derivative twice. For example, the second derivative of \(x^2\) with respect to \(x\) is \(2\), which can be found by taking the first derivative, getting \(2x\), and taking the first derivative again, getting \(2\).

More precisely, to compute the \(n\)th derivative of \(y\) with respect to \(x\),

\[\frac{d^0y}{dx^0} = y\]
\[\frac{d^ny}{dx^n} = \frac{d}{dx}\frac{d^{n-1}y}{dx}\]

where \(n \ge 0\).

Great! We have our @derivative function from part 1, so we can just call it \(n\) times, right? No. The signature doesn't line up. Our @derivative function returns a plain number, but the first argument has to be a @racket[DNumber]. Ok, can we just make @derivative return a @DNumber then?

Let's take a step back and think about what would mean. A @DNumber is the result of a differentiable computation. A @DNumber is created by storing the computation's inputs and the partial derivative of the output with respect to each input. An operator like multiplication is responsible for recording these inputs and computing these partial derivatives.

Currently, these partial derivatives are stored as plain numbers. And when we compute the derivative, we are adding and multiplying plain numbers together with Racket's built-in arithmetic operators, not our custom differentiable ones. In order for the result of @derivative to be a @racket[DNumber], the computation of the derivative itself must be differentiable. This means @racket[DNumber]s must store partial derivatives as @racket[DNumber]s instead of plain numbers and @derivative must use our differentiable addition and multiplication to combine partial derivatives. This also means that our operators, which construct a @DNumber for the result, will also have to construct a @DNumber to compute each partial derivative, rather than using plain Racket numbers.

Let's revise our data definitions:

@(define naive-eval (make-base-eval '(require racket)))
@#reader scribble/comment-reader
(examples #:eval naive-eval #:label #f
(struct dnumber [value inputs] #:transparent)
; a DNumber ("differentiable number") is a
; (dnumber Number (listof DChild) )
; It represents the result of a differentiable computation.
; `value` is the numerical value of the result of this computation
; `inputs` associates each input to this computation with the numerical value of its partial derivative
(struct dchild [input derivative] #:transparent)
; A DChild is a
; (dchild DNumber DNumber)
; It represents an input to a differential computation.
; `input` is the DNumber that was supplied as an input to the parent computation
; `derivative` is a DNumber representing the computation of the partial derivative of the parent result with respect to this input.
)

All we changed was the data type of the @derivative field of @racket[dchild]. It used to be a plain number, and not it's a @racket[DNumber].

Ok, so far so good. Let's rewrite multiplication and addition:

@#reader scribble/comment-reader
(examples
  #:eval naive-eval
  #:label #f
  (define const2 (dnumber 2 (list)))
  (define const3 (dnumber 3 (list)))
  ; (DNumber DNumber -> DNumber)
  ; differentiable multiplication
  (define (mul a b)
    (dnumber (* (dnumber-value a)
                (dnumber-value b))
             (list (dchild a b)
                   (dchild b a))))
  (mul const2 const3)
)

Let's compare this to the previous implementation:

@racketblock[
(define (mul/old a b)
  (dnumber (* (dnumber-value a)
              (dnumber-value b))
           (list (dchild a (dnumber-value b))
                 (dchild b (dnumber-value a)))))
]

Previously, the partial derivative with respect to one input was the numerical value of the other input. Now, we just use the other input directly! This is actually simpler and more straightforward than the old implementation.

Let's do addition now:

@#reader scribble/comment-reader
(examples
  #:eval naive-eval
  #:label #f
  (define const4 (dnumber 4 (list)))
  (define const5 (dnumber 5 (list)))
  ; (DNumber DNumber -> DNumber)
  ; differentiable addition
  (define (add a b)
    (dnumber (+ (dnumber-value a)
                (dnumber-value b))
             (list (dchild a (dnumber 1 (list)))
                   (dchild b (dnumber 1 (list))))))
  (add const4 const5)
)

The only change we made was creating dummy @racket[DNumber]s for the 1s.
@; TODO mention that they have to be different 1s?

@section{The Problem}

Things start to break down when we try to do this to an operator like \(\frac{1}{x}\):

\[
\frac{d}{dx} \frac{1}{x} = -\frac{1}{x^2}
\]

The derivative of the reciprocal operation contains another reciprocal operation. To see why this is a problem, let's look at the old and new implementations:


@racketblock[
(define (reciprocal-old x)
    (dnumber (/ 1 (dnumber-value x))
             (list (dchild x (/ -1 (* (dnumber-value x) (dnumber-value x)))))))
(define (reciprocal-new x)
    (dnumber (/ 1 (dnumber-value x))
             (list (dchild x (mul (dnumber -1 (list))
                                  (reciprocal-new (mul x x)))))))
]

Previously, we just use Racket's built-in @racket[/] function to compute the derivative. But now, since the computation of the derivative must be differentiable, we have to use our own operators to produce a @racket[DNumber]. For @racket[add] and @racket[mul], the partial derivatives are simple and don't involve other operations. But for @racket[reciprocal], the partial derivative is another call to @racket[reciprocal]. This recursion never terminates. This makes sense because you can keep taking the derivative of the reciprocal function and the exponent in the denominator will continue growing. Since the computation of the partial derivatives must be differentiable, they will have partial derivatives as well. If the function can be differentiated arbitrarily many times and continue producing different expressions with more partial derivatives, the computation graph will be infinite and its construction will never terminate.

@subsection{Laziness}

To avoid this problem, we can be lazy and construct the computation graph on-demand, rather than all at once. Remember, to compute first-order derivatives, all we needed was the numerical values of the partial derivatives. Similarly, to compute the second derivative, we only need the numerical value of the partial derivatives' partial derivatives. We can build the computation graph just as deeply as we need it, and no more.

To make this more concrete, let's explore a common example of laziness: streams.

A stream is like a list, but it can be infinite, and it can be constructed "on demand". For example, let's construct a stream containing all integers greater than or equal to a given one:

@examples[
  #:eval naive-eval
  #:label #f
  (require racket/stream)
  (define (integers-from n) (stream-cons n (integers-from (add1 n))))
  (stream->list (stream-take (integers-from 1) 10))
  (stream->list (stream-take (integers-from 1) 20))
]

@racket[stream-cons] is similar to @racket[cons] for lists, but delays the evaluation of the second argument until its value is needed. @racket[stream-take] takes in a stream and a number and produces another stream that produces that many elements from the beginning of the input stream and then ends. @racket[stream->list] forces the evaluation of all elements of the stream and converts the stream to a list. If we did @racket[(stream->list (integers-from 1))], it would never terminate.

Rather than store the entire stream in memory like a list, a stream stores a function in memory that computes the rest of the stream. However, once part of the stream is computed, it is remembered:

@examples[
  #:eval naive-eval
  #:label #f
  (define s (stream-cons 1 (stream-cons (begin (displayln "hello!") 2) empty-stream)))
  (stream->list s)
  (stream->list s)
]

The @racket[(displayln "hello!")] only runs once because the first @racket[stream->list] forces the evaluation of the whole stream and remembers its elements. The second @racket[stream->list] just uses the values that were already computed and stored, rather than computing them again.

This is the kind of behavior we want for our new computation graphs. Like a stream, our computation graph might be infinite, but we don't need the whole thing at once. And we don't want to re-run potentially expensive numerical computations if we don't have to.

We will achieve this laziness using Racket's promises. From the Racket documentation:

@nested{
A promise encapsulates an expression to be evaluated on
demand via @racket[force]. After a promise has been @racket[force]d,
every later @racket[force] of the promise produces the same result.
}

For our purposes, we will be using @racket[delay], which creates this kind of promise. For information about another kind of promise, @racket[lazy], see @link["/blog/2022/10/02/composable-promises.html"]{my blog post on composable promises}!

Here is an example showing how @racket[delay] and @racket[force] work:

@examples[
  #:eval naive-eval
  #:label #f
  (require racket/promise)
  (define p (delay (displayln "hello!") 42))
  (force p)
  (force p)
]

Like a stream's elements, the value of the promise is computed only when forced, and the result is remembered so subsequent forces don't re-compute the result.

For our computation graphs, what should we wrap in a promise? In other words, what should only be computed on demand? Sometimes, we don't care about a @racket[DNumber]'s derivatives at all. For example, if we just want a @racket[DNumber]'s value, we don't need any of its derivatives. But when we are computing the derivative of a @DNumber with respect to another, we will look at all of its derivatives. Considering this, it makes sense to wrap the @racket[children] list of a @DNumber in a promise. This means that the only thing that will be eagerly computed when constructing a @DNumber is its value. Derivatives will only be computed on-demand. And when they are @racket[force]d, they'll all be forced at once, and only their values will be computed eagerly. If we want the first derivative, we won't even compute partial derivatives of partial derivatives. We'll only compute the immediate values of the immediate partial derivatives.

@section{Lazy Computation Graph}

@(define lazy-eval (make-base-eval '(require racket/base racket/promise)))

Here are our new data definitions:

@#reader scribble/comment-reader
(examples #:eval lazy-eval #:label #f
(struct dnumber [value children] #:transparent)
; A DNumber is a
; (dnumber number? (promise/c (listof DChild)))
; Represents the result of a differentiable computation
; where
; value is the numerical result (plain number)
; children are the inputs and their derivatives.

(struct dchild [input derivative] #:transparent)
; A DChild is a
; (dchild DNumber DNumber)
; Represents an input to a differentiable computation and its derivative
; where
; input is the input
; derivative is its first derivative of this child's parent with respect to this input
)

Rather than a list of @racket[DChild]ren, we store a promise of a list of @racket[DChild]ren. This achieves the desired laziness. Let's rewrite multiplication and addition again:

@#reader scribble/comment-reader
(examples
  #:eval lazy-eval
  #:label #f
  (define const2 (dnumber 2 (delay (list))))
  (define const3 (dnumber 3 (delay (list))))
  ; (DNumber DNumber -> DNumber)
  ; differentiable multiplication
  (define (mul a b)
    (dnumber (* (dnumber-value a)
                (dnumber-value b))
             (delay (list (dchild a b)
                          (dchild b a)))))
  (mul const2 const3)
  (define const4 (dnumber 4 (delay (list))))
  (define const5 (dnumber 5 (delay (list))))
  ; (DNumber DNumber -> DNumber)
  ; differentiable addition
  (define (add a b)
    (dnumber (+ (dnumber-value a)
                (dnumber-value b))
             (delay (list (dchild a (dnumber 1 (delay (list))))
                          (dchild b (dnumber 1 (delay (list))))))))
  (add const4 const5)
)

It's the same as before, except we wrap the @racket[list]s in a @racket[delay]. Now let's write @racket[reciprocal]:

@#reader scribble/comment-reader
(examples
  #:eval lazy-eval
  #:label #f
  (define (reciprocal x)
    (dnumber (/ 1 (dnumber-value x))
             (delay (list (dchild x (mul (dnumber -1 (list))
                                         (reciprocal (mul x x))))))))
)

Again, we just wrap in a @racket[delay]. However, in this case, the @racket[delay] prevents an infinite recursion. The evaluation of the list of children is delayed until it is @racket[force]d, so the function terminates without actually making a recursive call. Like the infinite stream of integers, this infinite computation graph is computed on-demand.

Now let's re-write @racket[derivative]:

@#reader scribble/comment-reader
(examples #:eval lazy-eval #:label #f
; (DNumber DNumber [natural?] -> DNumber)
; computes the nth derivative of y with respect to x
(define (derivative y x [n 1])
  ; single derivative
  (define (d/dx y)
    (if (eq? y x)
        (dnumber 1 (delay (list)))
        (let ([inputs (force (dnumber-children y))])
          (for/fold ([sum (dnumber 0 (delay (list)))])
                    ([input inputs])
            (let ([u (dchild-input input)]
                  [dy/du (dchild-derivative input)])
              (add sum (mul dy/du (d/dx u))))))))
  ; apply d/dx n times
  (let loop ([n n] [y y])
    (if (zero? n)
        y
        (loop (sub1 n) (d/dx y)))))
)

The function @racket[d/dx] is like our original @derivative function, except it always uses the outer @racket[x]. It computes the derivative of the given @racket[y] with respect to the outer @racket[x]. In Racket, variable names can have a slashes in them, so there is nothing special about the name @racket[d/dx]. There are a few differences between the old @derivative function and @racket[d/dx]: Instead of returning @racket[1], we create a fresh constant @DNumber for it. And since @racket[dnumber-children] is a promise, we must @racket[force] it to demand the values of the children. Additionally, since we're working with @racket[DNumber]s instead of plain numbers, we can't use Racket's @racket[for/sum]. Instead, we use @racket[for/fold], which accumulates the @racket[sum] variable throughout the loops. We use our custom @racket[add] operator to compute the rolling sum and we use our @racket[mul] operator to compute the chain rule.

The part after the @racket[d/dx] function is a little loop that calls @racket[d/dx] @racket[n] times on @racket[y]. That corresponds to our definition of the \(n\)th derivative as repeated application of the first derivative.

Let's run the same tests as before:

@examples[
  #:eval lazy-eval
  #:label #f
  (derivative const4 const4)
  (derivative const4 const3)
  (derivative const4 (dnumber 4 (delay (list))))
  (derivative (add const4 const4) const4)
  (derivative (add const4 const4) (dnumber 4 (delay (list))))
  (define (add-4 x) (add x (dnumber 4 (list))))
  (derivative (add-4 const3) const3)
  (derivative (add-4 const4) const4)
  (define (double x) (add x x))
  (derivative (double const3) const3)
  (derivative (double const3) const4)
  (define (square x) (mul x x))
  (derivative (square const4) const4)
  (derivative (square const3) const3)
  (derivative (mul const3 const4) const4)
  (derivative (mul const3 const4) const3)
  (derivative (square (add (mul const3 const4) const2))
              const4)
]

The results are the same, but @derivative returns @racket[DNumber]s now instead of plain numbers. Now let's compute some higher order derivatives:

@examples[
  #:eval lazy-eval
  #:label #f
  (derivative (square const4) const4)
  (derivative (square const4) const4 2)
  (derivative (square const4) const4 3)
  (derivative (square const4) const4 0)
]

Nice! One nice thing about promises is that when we print them out, we can see whether they're forced or not:

@examples[
  #:eval lazy-eval
  #:label #f
  (define p (delay 2))
  p
  (force p)
  p
]

If the promise is forced, we see its value.

We can use this to see how much of our computation graph has been forced:

@examples[
  #:eval lazy-eval
  #:label #f
  (define recip4 (reciprocal const4))
  recip4
  (derivative recip4 const4)
  recip4
  (derivative recip4 const4 2)
  recip4
]

We can see that, initially, no derivatives have been computed. But as we take higher order derivatives, more of the compuation graph gets forced as needed.

@;TODO more operators
@;TODO e^x sharing
@;TODO it's so nice that racket has a library for laziness that you can just slap in there
