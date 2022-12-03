#lang scribble/manual

Title: Automatic Differentiation
Date: 2022-11-26T18:24:05
Tags: racket, math, machine-learning, projects

@require[scribble/example scribble/html frog/scribble @for-label[racket]]
@(define eval (make-base-eval '(require racket)))

Automatic differentiation is a technique that allows programs to compute the derivatives of functions. It is vital
for deep learning and useful for optimization in general.
For me, it's always been dark magic, but I recently thought of a way to implement it and made a little library. This
blog post takes you along the journey of discovering it.

This post requires some knowledge of differential calculus. You'll need to know basic derivative rules, the chain rule,
and partial derivatives. If you've taken an introductory calculus course, you should be fine.

The code is in Racket. If you don't know racket, you should still be able to follow along. I'll explain the Racket-y stuff.

<!-- more -->

@table-of-contents[]

@section{Introduction}

Gradient descent is an optimization technique that involves derivatives. You have some quantity you want to optimize,
let's say \(y\), and you have some variable \(x\) that you can adjust that affects \(y\). What value of \(x\) maximizes \(y\)?
Using gradient descent, if we take the derivative of \(y\) with respect to \(x\), \(\frac{dy}{dx}\), that tells us how changing \(x\)
affects \(y\). If the derivative is positive, increasing \(x\) increases \(y\). If it is negative, increasing \(x\) decreases \(y\).
If it is positive and large, increasing \(x\) increases \(y\) a lot. So, if we're trying to maximize \(y\), we'd change \(x\) in the same
direction as the derivative. If it's positive, we increase \(x\) to make \(y\) greater. If it's negative, we decrease \(x\) to make \(y\)
greater. This will end up maximizing \(y\) (at least, reaching a local maximum). This is a very useful technique and it is fundamental to
deep learning with neural networks. Awesome! But how do you compute the derivative of a function with respect to its input?

Naively, you might try something like this:

@examples[
  #:label #f
  #:eval eval
  (define (derivative f x #:dx [dx 0.01])
    (/ (- (f (+ x dx)) (f x))
       dx))
  (derivative (lambda (x) (* x x)) 5)
]

For those of you not familiar with Racket, it has prefix arithmetic. This means that instead of writing \(a + b\), we write @racket[(+ a b)].

This is reminiscent of the limit definition of a derivative that you learn about in an introductory calculus course.

\[\frac{df}{dx} = \lim_{h \to 0} \frac{f(x + h) - f(x)}{h}\]

This can work well enough, but you need a small @racket[dx] and there will always be rounding error. Another issue is that
if we have a multi-argument function, we'd have to run the function many times to get the partial derivatives.
This is not ideal. What we'd like is to be able to run the function once, inspect several derivatives after, and get exact derivatives.

Here is a sneak peek of what we will implement:

@examples[
  #:label #f
  #:eval eval
  (require number-diff)
  (define (f a b) (*o a b))
  (define a (number->dnumber 5))
  (define b (number->dnumber 3))
  (define y (f a b))
  (dnumber->number y)
  (dnumber->number (derivative y a))
  (dnumber->number (derivative y b))
]

We'll even get higher order derivatives!

@examples[
  #:label #f
  #:eval eval
  (define y (*o a a))
  (dnumber->number y)
  (dnumber->number (derivative y a))
  (dnumber->number (derivative (derivative y a) a))
  (dnumber->number (derivative y a #:order 2))
]

@section{How?}

@subsection{The Math}

I first encountered automatic differentiation when I was learning about neural networks in college.
In the popular neural networks libraries, you define your neural network, and this constructs a computation
graph. A computation graph stores the operations, inputs, and outputs of a computation. Here is an example
from PyTorch documentation:

@image["img/computation-graph.png"]{computation graph}

@hyperlink["https://pytorch.org/blog/computational-graphs-constructed-in-pytorch/"]{source}

This represents the computation \(\log (x_1 x_2) \sin (x_1)\). Intermediate results like \(x_1 x_2\) get their own nodes (\(a\)).

The green part of the image shows how the derivatives are calculated. Each operator, like multiplication, logarithm, etc., knows
how to compute the derivatives of its result with respect to its inputs. The rest is just applying the chain rule and adding up
partial derivatives as appropriate.

There is a lot going on here, so don't worry about fully understanding this picture yet. We will get there. The key idea is that
a computation can be expressed as a graph, where inputs are like leaves, and operations are like nodes. And each node is a tiny computation
which is easy to compute the derivatives for.

Let's start by thinking about some core operators and their derivatives:

\[
mul(a,b) = ab
\]

What are the derivatives?

\[
\frac{\partial mul}{\partial a} = b
\]

\[
\frac{\partial mul}{\partial b} = a
\]

This comes from the rule for multiplying a constant:

\[
\frac{d}{dx} cx = c
\]

Remember, taking the partial derivative means treating the other arguments as constants.

Ok, what about addition?

\[
add(a,b) = a + b
\]

\[
\frac{\partial add}{\partial a} = 1
\]

The derivative is 1 because \(\frac{da}{da} = 1\) and \(\frac{da}{db} = 0\). Using the sum rule, we get \(1+0=1\).

\[
\frac{\partial add}{\partial b} = 1
\]


Exponentiation?

\[
expt(a,b) = a^b
\]

\[
\frac{\partial expt}{\partial a} = ba^{b-1}
\]

This derivative treats \(b\) as a constant, which is like a polynomial. So we use the power rule.

\[
\frac{\partial expt}{\partial b} = a^b \ln(a)
\]

This derivative treats \(a\) as a constant, which is like an exponential. So we use the exponential rule.

What if things get more complicated? How would you compute this derivative?

\[
y = (3x + 1)^2
\]

\[
\frac{dy}{dx} = ?
\]

You use the chain rule!

\[
\frac{dy}{dx} = \frac{dy}{du} \frac{du}{dx}
\]

Let \(u_1 = 3x+1\).

\[y = u_1^2\]

Now we can use the power rule:

\[
\frac{dy}{du_1} = 2u_1
\]

We can easily tell that \(\frac{du_1}{dx} = 3\), but let's do this mechanically to get an idea of how we might automate it:

Let \(u_2 = 3x\)

\[u_1 = u_2 + 1\]

Now we can use the derivative of \(add\):

\[
\frac{du_1}{du_2} = 1
\]

Finally, we use the constant factor rule:

\[\frac{du_2}{dx} = 3\]

Now, we can go back up the chain:

\[\frac{du_1}{dx} = \frac{du_1}{du_2}\frac{du_2}{dx} = 1 \cdot 3 = 3\]

\[\frac{dy}{dx} = \frac{dy}{du_1}\frac{du_1}{dx} = 2u_1 \cdot 3 = 6u_1 = 6(3x+1)\]

We did it!

We almost have a recursive algorithm. There's just one tricky bit: What if \(x\) shows up twice?

In other words, if we have \(f(a,b)\) and we know \(\frac{\partial f}{\partial a}\) and \(\frac{\partial f}{\partial b}\), how do we
compute \(\frac{df(x,x)}{dx}\)? We can just add the partial derivatives!

Intuitively, if we think about the derivative as "how does adjusting the input(s) affect the output?", then if \(x\) shows up in multiple places, we
can think of this situation as making several inputs change the same way and seeing how the output is affected. If we know how each input changes the output,
we just add up all of those little changes to get the big, total change.

Here is an example:

\[\frac{d add(x,x)}{dx} = 1 + 1 = 2\]

The partial derivative of \(add\) with respect to each input is 1. So we get \(1+1=2\). This makes sense because \(x+x=2x\) and \(\frac{d2x}{dx} = 2\).

One more example:

\[\frac{d mul(x,x)}{dx} = x + x = 2x\]

The partial derivative of \(mul\) with respect to each input is the other input. So we get \(x + x = 2x\). This makes sense because \(x \cdot x = x^2\) and \(\frac{dx^2}{dx} = 2x\).

In general, for computing \(\frac{\partial f(a,b)}{\partial x}\), \(a\) and \(b\) might be \(x\), might depend on \(x\), or might not depend on \(x\) at all.
To account for this, we use the chain rule and this "partial derivative sum rule" (if there is a name for this, please let me know!):

\[\frac{\partial f(a,b)}{\partial x} = \frac{\partial f(a,b)}{\partial a}\frac{\partial a}{\partial x} + \frac{\partial f(a,b)}{\partial b}\frac{\partial b}{\partial x}\]

If \(a = x\), \(\frac{\partial a}{\partial x} = 1\). If \(a\) depends on \(x\), \(\frac{\partial a}{\partial x}\) be some value. If it does not depend on \(x\), it will be 0.

For example, let's compute \(\frac{d}{dx} (5x)^2\):

\[\frac{d expt(5x,2)}{dx} = \frac{d expt(5x,2)}{d5x}\frac{d5x}{dx} + \frac{d expt(5x,2)}{d2}\frac{d2}{dx}\]
\[\frac{d expt(5x,2)}{dx} = 2 \cdot 5x \cdot 5 + \frac{d expt(5x,2)}{d2} \cdot 0\]
\[\frac{d expt(5x,2)}{dx} = 50x\]

Notice that, since 2 does not depend on \(x\), its derivative was 0, and that term did not contribute to the overall derivative.

Normally, you don't think of something like \((5x)^2\) as a sum of two partial derivatives like this. You don't bother taking the
derivative of the 2. Normally, you don't have to worry about it since it just ends up adding 0. But since we're trying to automate this, we need to be general and
account for the possibility that \(x\) might show up in the base @emph{and} the exponent when differentiating an exponential, or any function. In fact, we should've
done the same thing for \(\frac{d5x}{dx}\) and added up \(\frac{dmul(5,x)}{dx}\frac{dx}{dx} and \frac{dmul(5,x)}{d5}\frac{d5}{dx}\) to get \(5 \cdot 1 + x \cdot 0 = 5\).

You may have been wondering, "where's the product rule?" In fact, the product rule is not fundamental! It can be derived from the constant factor rule,
the chain rule, and this "partial derivative sum rule":

\[\frac{d}{dx}f(x)g(x) = \frac{d}{dx}mul(f(x),g(x)) = \frac{\partial mul(f,g)}{\partial f}\frac{df}{dx} + \frac{\partial mul(f,g)}{\partial g}\frac{dg}{dx}\]
\[\frac{d}{dx}f(x)g(x) = g\frac{df}{dx} + f\frac{dg}{dx}\]

Nice!

Now, we're ready for the recursive algorithm to compute (partial) derivatives.

\[derivative(x,x) = 1\]
\[derivative(y,x) = 0, y \ne x\]
where \(y\) is a variable
\[derivative(c,x) = 0\]
where \(c\) is a constant
\[derivative(f(u_1,u_2, \cdots , u_n), x) = \sum_{i=0}^{n} derivative(f(u_1,u_2, \cdots , u_n), u_i) \cdot derivative(u_i, x)\]

The base cases are the constant rule and the fact that \(\frac{dx}{dx} = 1\).
The recursive case is the interesting bit. For each input to the computation, we apply the chain rule, which involves two recursive calls. And we add up all of those partial derivatives.

This algorithm has two big issues: Firstly, since the first argument is unchanged in the recursive call, we will get infinite recursion.
Secondly, \(u_i\) may be a complex expression, so how do we compute the derivative with respect to it?

To solve these problems, we take a shortcut. We don't actually need a recursive call here. If \(f\) is a function and we know
how to compute the partial derivative of \(f\) with respect to each of its inputs, we can just use that information here directly and
replace the derivative with the result of the function's partial derivative.

This is all very hand-wavy, but it'll become more concrete soon, I promise!

At this point, that computation graph picture from earlier should start to make some sense. In particular, the green part.
Each operator knows how to compute the derivatives of its result with respect to its inputs. That's the "backward" stuff.
And it says "Grads from different paths are added together" (grad=gradient=derivative for our purposes) because of that "partial derivative sum rule". In this example,
\(x_2\) shows up twice in the computation, so we need to account for that by adding multiple partial derivatives together.


@subsection{The Implementation}

We have a (somewhat hand-wavy) algorithm for computing derivatives. Now, we have to actually implement it.

@subsubsection{A Data Representation}

Let's think about the pieces we'll need:

We'll need operators which can compute partial derivatives of their result with respect to their input(s).
We'll also need constants to pass to these operators. It is unclear how "variables" will be represented, so let's not think about that for now.
In order to compute the derivative of a result with respect to some input after the computation is complete, we'll need to remember what inputs
were involved in a computation.

So a computation will need to store the value of its result and its inputs, at a bare minimum. Its inputs may be results of other computations too.
This means that there will be a tree structure where a computation has a node that stores its result and has children for its inputs. At the leaves of this
tree will be constants. We can think of a constant as a computation with no inputs that has itself as a result. Let's write a data definition for this tree:

@#reader scribble/comment-reader
(racketblock
(struct computation [value inputs] #:transparent)
; A Computation is a
; (computation Number (listof Computation))
; It represents the result of a numerical computation
; Examples:
(define const2 (computation 2 (list)))
(define const3 (computation 3 (list)))
(define prod23 (computation 6 (list const2 const3)))
)

In Racket, @racket[struct] creates a structure type. It's like a struct in C, or a data class in Python or Java. It only has fields, no methods.
Semicolon creates a line comment and @racket[#:transparent] automatically implements structural equality and string rendering for our structure type.

This is on the right track, but it's not enough information to compute derivatives. We have no way to compute the derivative
of the result with respect to an input. What else do we need in the tree?

Let's take another look at the recursive case of our algorithm. The recursive step involves computing the partial derivatives of the result with respect
to one of its direct inputs, \(\frac{\partial f(u_1,u_2, \cdots , u_n)}{\partial u_i}\). This is the missing piece. We just have to figure out how to represent
this information in our tree.

One option would be to do what PyTorch seems to do: Store some information about the operation in a node. In our @racket[prod23] example,
we'd store something representing multiplication in the tree. In the PyTorch, we see that the computation graph stores something for @racket[*] and something called
@racket[MultBackward]. @racket[MultBackward] is something that knows how to compute the derivative of a product with respect to its input factors. With this design,
we could store a function in the tree that can be used to compute the derivative of the result with respect to each input. Each operator would be responsible for
computing its result and creating a node containing that result, a function for computing derivatives, and the inputs.

Another option is to pre-compute these derivatives and store the value of each derivative in the tree with its corresponding input.
With this design, each operator would be responsible for
computing its result,
computing the derivative of the result with respect to each input,
and creating a node containing the result, the inputs, and the derivatives of the result with respect to each input.
This is the design I chose for my implementation, and the one we'll implement together.

Both options have pros and cons. This is just the design I came up with.
One good thing about this design is that it generalizes nicely to higher order derivatives, which was a goal of my implementation.

Let's think about an example to make this more concrete. Let's say we have some operator @racket[f] and some inputs @racket[a] and @racket[b].
The computation is @racket[(f a b)] (that's how we write \(f(a,b)\) in Racket).
Let's call the result @racket[y]. Keep in mind that @racket[a], @racket[b], and @racket[y] are not plain numbers.
They are our trees. In this case, @racket[a] and @racket[b] will be direct children of @racket[y] since they are inputs to the computation that produced @racket[y].
The tree for @racket[y] will store the numerical value of the result of the computation, and for its children, it will have @racket[a] and @racket[b]. It will also
store the numerical value of \(\frac{\partial y}{\partial a}\) and the numerical value of \(\frac{\partial y}{\partial b}\).

If we're trying to compute @racket[(derivative y x)], where @racket[x] may be some input to the computation that produced @racket[a],
the first thing we'll encounter is the node @racket[y], which came from @racket[(f a b)], and we'll do the recursive step.
According to our algorithm, we need to compute

\[\frac{\partial y}{\partial a} \cdot derivative(a,x) + \frac{\partial y}{\partial b} \cdot derivative(b,x)\]

Those partial derivatives are what we have in our tree. The rest is just making recursive calls and applying the chain rule. Now we have enough information
to compute derivatives! Let's write a data definition:

@(define first-order-eval (make-base-eval '(require racket)))
@#reader scribble/comment-reader
(examples #:eval first-order-eval #:label #f #:no-prompt
(struct dnumber [value inputs] #:transparent)
; a DNumber ("differentiable number") is a
; (dnumber Number (listof DChild) )
; It represents the result of a differentiable computation.
; `value` is the numerical value of the result of this computation
; `inputs` associates each input to this computation with the numerical value of its partial derivative
(struct dchild [input derivative] #:transparent)
; A DChild is a
; (dchild DNumber Number)
; It represents an input to a differential computation.
; `input` is the DNumber that was supplied as an input to the parent computation
; `derivative` is the numerical value of the partial derivative of the parent result with respect to this input.
)

A @racket[DNumber] stores the value of its result as a plain number and, for each input, the input's DNumber and the partial derivative of
the result with respect to that input as a plain number.

Let's look at \(2 \cdot 3\) as an example:

@examples[
  #:eval first-order-eval
  #:label #f
  #:no-prompt
  (define const2 (dnumber 2 (list)))
  (define const3 (dnumber 3 (list)))
  (define prod23 (dnumber 6 (list (dchild const2 3) (dchild const3 2))))
]

We create @racket[dnumber]s for the constants 2 and 3. Since these are constants, they have no children (@racket[(list)] creates an empty list).

We then construct a node for the multiplication which stores the value of the result (6) and each input paired with its derivative.

The result of \(2 \cdot 3\) is 6. The inputs are 2 and 3. Recall the derivatives of the \(mul\) multiplication operator:

\[\frac{\partial mul(a,b)}{\partial a} = b\]
\[\frac{\partial mul(a,b)}{\partial b} = a\]

The derivative of the product with respect to one of its factors is the other factor.
So the derivative of @racket[prod23] with respect to @racket[const2] is the plain number 3.

We are pre-computing that \(derivative(f(u_1,u_2, \cdots, u_n), u_i)\) from our algorithm
and storing it directly in our tree as the @racket[derivative] field (the second argument of the constructor) of a @racket[dchild].

Let's implement our multiplication operator:

@#reader scribble/comment-reader
@examples[
  #:eval first-order-eval
  #:label #f
  ; (DNumber DNumber -> DNumber)
  ; differentiable multiplication
  (define (mul a b)
    (dnumber (* (dnumber-value a)
                (dnumber-value b))
             (list (dchild a (dnumber-value b))
                   (dchild b (dnumber-value a)))))
  (mul const2 const3)
  prod23
]

The function @racket[dnumber-value] is a field-accessor function that is automatically generated from the @racket[struct] declaration. This field contains the numerical result of the computation.
Since Racket's @racket[*] function expects plain numbers, we have to get the numerical values of the inputs with @racket[dnumber-value]
before passing them to @racket[*].

We also have to do this when creating the @racket[dchild]ren.
This is because the @racket[derivative] field of a @racket[dchild] must be a plain number.
However, the @racket[input] field must be a DNumber,
so we pass the input itself in as the first argument to the @racket[dchild] constructor and the numerical value of the other input as the second argument.

Let's do another example, this time \(4 + 5\):

@examples[
  #:eval first-order-eval
  #:label #f
  #:no-prompt
  (define const4 (dnumber 4 (list)))
  (define const5 (dnumber 5 (list)))
  (define sum45 (dnumber 9 (list (dchild const4 1) (dchild const5 1))))
]

Recall the derivatives of the \(add\) addition operator:

\[\frac{\partial add(a,b)}{\partial a} = 1\]
\[\frac{\partial add(a,b)}{\partial b} = 1\]

Now let's implement it:

@examples[
  #:eval first-order-eval
  #:label #f
  ; (DNumber DNumber -> DNumber)
  ; differentiable addition
  (define (add a b)
    (dnumber (+ (dnumber-value a)
                (dnumber-value b))
             (list (dchild a 1)
                   (dchild b 1))))
  (add const4 const5)
  sum45
]

Great! Now we have a few differentiable operators. Adding more operators will be just like this. We extract the values from the arguments,
compute the result using the inputs' values and built-in arithmetic operators from Racket,
and then create a list of @racket[dchild]ren mapping each input to the value of the derivative
of the result with respect to that input.

Now we're finally ready to start implementing the @racket[derivative] function!

@subsubsection{The Derivative!}

@;TODO address reference equality and the fact that it's actually a DAG, not a tree. Show an example like x + x
