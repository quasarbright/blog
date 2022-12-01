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

To solve these problems, we take a shortcut. We don't actually need a recursive call here. If \(f\) is an function and we know
how to compute the partial derivatives of \(f\) with respect to its inputs, we can just use that information here directly and
replace the derivative with the result of the function's partial derivative.

This is all very hand-wavy, but it'll become more concrete soon, I promise!

At this point, that computation graph picture from earlier should start to make some sense. In particular, the green part.
Each operator knows how to compute the derivatives of its result with respect to its inputs. That's the "backward" stuff.
And it says "Grads from different paths are added together" (grad=gradient=derivative for our purposes) because of that "partial derivative sum rule". In this example,
\(x_2\) shows up twice in the computation, so we need to account for that by adding multiple partial derivatives together.


@subsection{The Implementation}

We have a (somewhat hand-wavy) algorithm for computing derivatives. Now, we have to actually implement it.

Let's think about the pieces we'll need:

We'll need operators which can compute partial derivatives of their result with respect to their input(s).
We'll also need constants to pass to these operators. It is unclear how "variables" will be represented, so let's not think about that for now.
In order to compute the derivative of a result with respect to some input after the computation is complete, we'll need to remember how inputs
were involved in computations.

Normally, when you perform a numerical computation, you have some constants, which are numbers, and you perform some operations, which take in one or several numbers
and output a single number. A number is an atomic piece of data and has no record of the operations or inputs of the computation that produced it. That's not good for us
because we want to take the result of the computation and find derivatives after it completed. We're going to do something similar to PyTorch and store a computation graph.

We will represent the result of a computation as a tree. Its leaves will be constants and its nodes will correspond to operations like addition and multiplication. Now we
have to make a decision about what we store in the nodes. One option is to store information about the operation, like what PyTorch seems to do, based on the computation graph.
PyTorch seems to create a node for the multiplication which stores its inputs, the fact that the operation was a multiplication, and something called "MultBackward", which has
something to do with computing the derivatives. We're going to do something a little different.

Our implementation will not care about what operation was performed, only the inputs and their derivatives. We're only concerned with computing derivatives and want
little overhead.

Let's take another look at the recursive case of our algorithm. The recursive step just involves computing the immediate partial derivatives of the result with respect
to its direct inputs. All we need are the values of the derivatives and some way of associating each input with the value of its derivative. We don't even need to inspect the value of
the inputs (or the result), just a way to get the value of the derivative for an input.

Let's think about an example to make this more concrete. Let's say we have some operator @racket[f] and some inputs @racket[a] and @racket[b]. Let's call the result of @racket[(f a b)]
@racket[y] (that's how we write \(f(a,b)\) in Racket). Keep in mind that @racket[a], @racket[b], and @racket[y] are not plain numbers. They are computation graphs, whatever those are. In this case, @racket[a] and
@racket[b] will be direct children of @racket[y] in some sense since they are inputs to the operation that produced @racket[y].

If we're trying to compute @racket[(derivative y x)], the first thing we'll encounter is a node corresponding to @racket[(f a b)], and we'll do the recursive step.
We first have to compute the derivative of @racket[(f a b)] with respect to @racket[a].
@racket[y] (a computation graph) will have stored something mapping @racket[a] (a computation graph)
to the value of the partial derivative of @racket[(f a b)] with respect to @racket[a] (a plain number). That's our \(derivative(f(u_1,u_2, \cdots, u_n), u_i)\).
We can then recur with @racket[(derivative a x)]. That's our \(derivative(u_i, x)\).
