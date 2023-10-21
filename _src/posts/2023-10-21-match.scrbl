#lang scribble/manual

Title: Understanding and Implementing Pattern Matching
Date: 2023-10-21T00:44:54
Tags: UNLINKED, racket, tutorials, programming-languages

@require[
  scribble/example
  @for-label[racket]]
@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

Pattern matching is a very powerful tool used to destructure and perform case analysis on data.
It's commonly found in functional languages and has recently made its way into Python. In this post,
we'll discover pattern matching and implement it in Racket.

I will assume that you have some familiarity with Racket.

<!-- more -->

@table-of-contents[]

@section{Motivation}

If you're already familiar with pattern matching, feel free to skip to @seclink["Implementation"]{the implementation}.

Before we get to pattern matching, let's talk about trees. Let's say we're trying to find the largest element in a binary tree.
You can do this with predicates and accessors:

@repl[
(struct node [left right] #:transparent)
(struct leaf [data] #:transparent)
(define (bt-max bt)
  (cond
    [(node? bt) (max (bt-max (node-left bt)) (bt-max (node-right bt)))]
    [(leaf? bt) (leaf-data bt)]))
(bt-max (leaf 1))
(bt-max (node (leaf 1) (node (leaf 3) (leaf 2))))
]

Easy enough. Now, let's reflect a binary tree to create its mirror image:

@repl[
(define (bt-reflect bt)
  (cond
    [(node? bt)
     (node (bt-reflect (node-right bt))
           (bt-reflect (node-left bt)))]
    [(leaf? bt) bt]))
(bt-reflect (leaf 1))
(bt-reflect (node (leaf 1) (leaf 2)))
(bt-reflect (node (leaf 1) (node (leaf 2) (leaf 3))))
]

This looks pretty similar to the previous function. In fact, it's not hard to imagine pretty much every
function on trees looking just like this: Check if it's a node with @racket[node?] use field accessors to get the left and right
subtrees, check if its a leaf with @racket[leaf?], and use field accessors to get the data.

Let's be good little programmers and avoid repeating ourselves by creating an abstraction:

@repl[
(define (bt-cases bt on-node on-leaf)
  (cond
    [(node? bt) (on-node (node-left bt) (node-right bt))]
    [(leaf? bt) (on-leaf (leaf-data bt))]))
(define (bt-max bt)
  (bt-cases bt
    (lambda (left right) (max (bt-max left) (bt-max right)))
    (lambda (data) data)))
(bt-max (leaf 1))
(bt-max (node (leaf 1) (node (leaf 3) (leaf 2))))
]

This is a little cleaner. It got rid of the predicates and accessors,
but there is still a little boilerplate with those lambdas.
To fix this, we can go one step further and make a macro!

@repl[
(define-syntax-rule
  (bt-match bt [(left right) node-body ...] [(data) leaf-body ...])
  (bt-cases bt
    (lambda (left right) node-body ...)
    (lambda (data) leaf-body ...)))
(define (bt-max bt)
  (bt-match bt
    [(left right) (max (bt-max left) (bt-max right))]
    [(data) data]))
(bt-max (leaf 1))
(bt-max (node (leaf 1) (node (leaf 3) (leaf 2))))
]

Very nice! We have a concise syntax for defining functions on binary trees. One limitation is that we can't look deeper than one
level into the data structure. If we were doing something like tree rotations, we'd need to look 2 levels into the structure, but these
tools wouldn't support that. Another limitation is that this only works for binary trees! Do we have to make this every time we work with union data?

Now let's look at some examples with lists. First, let's split a list into pairs:

@repl[
(define (to-pairs lst)
  (if (<= 2 (length lst))
      (cons (list (first lst) (second lst)) (to-pairs (rest (rest lst))))
      '()))
(to-pairs '())
(to-pairs '(1))
(to-pairs '(1 2))
(to-pairs '(1 2 3))
(to-pairs '(1 2 3 4))
(to-pairs '(1 2 3 4 5))
(to-pairs '(1 2 3 4 5 6))
]

Now let's zip two lists together:

@repl[
(define (zip xs ys)
  (cond
    [(and (cons? xs) (cons? ys))
     (cons (list (first xs) (first ys)) (zip (rest xs) (rest ys)))]
    [else '()]))
(zip '() '())
(zip '(1) '())
(zip '(1) '(a))
(zip '(1 2 3) '(a b c))
(zip '(1 2) '(a b c d))
]

There is a similar pattern, but more general: We have a few possible cases for the structure of our data, we check the possible cases of
the structure of our data, and based on the structure, we extract pieces of our data and operate on them. Except this
isn't as straightforward and abstract-able as our very repetitive binary tree operations. There is still an abstraction to be made, but
it's a much more general and powerful one. This abstraction is, of course, pattern matching.

@section{@racket[match]}

Here is how we would implement a tree operation with pattern matching:

@repl[
(define (bt-max bt)
  (match bt
    [(node left right) (max (bt-max left) (bt-max right))]
    [(leaf data) data]))
(bt-max (leaf 1))
(bt-max (node (leaf 1) (node (leaf 3) (leaf 2))))
]

It's pretty much the same as our @racket[bt-match], except now we have to specify which constructor we're matching in which case.

The general form for using @racket[match] is @racket[(match val [pattern body] ...)] where @racket[val] is the value you're destructuring,
@racket[pattern] is a pattern, which specifies the structure of the data for this case and may bind variables to its fields, and @racket[body]
has access to these fields. A @racket[match] form can have many cases. The first case with a pattern that matches the structure of the data
binds the variables in its pattern to the corresponding pieces of @racket[val] and runs that case's @racket[body].

In this example, we have a @racket[node] pattern which binds the subtrees to variables called @racket[left] and @racket[right]. This pattern matches
when the value being matched is a node and matches the sub-patterns against the sub-trees.
Variable patterns like @racket[left] match any type of value and bind the value to that variable for use in the @racket[body].

Here is how we would implement the list operations with pattern matching:

@repl[
(define (to-pairs lst)
  (match lst
    [(cons x (cons y lst))
     (cons (list x y) (to-pairs lst))]
    [_ '()]))
(define (zip xs ys)
  (match (list xs ys)
    [(list (cons x xs) (cons y ys))
     (cons (list x y) (zip xs ys))]
    [_ '()]))
]

In the @racket[to-pairs] example, we first check for the case of a list with at least two elements by using two @racket[cons] patterns.
A @racket[cons] matches values that are @racket[cons] cells and matches the first sub-pattern agains the @racket[car] and the second against
the @racket[cdr] of the value.

We also see the underscore pattern @racket[_], which matches against any value, like a variable pattern, and ignores the value.
It is often used like @racket[else] in a cond, but can also be used to ignore a field as a subpattern.

Here, we have a usage of a nested pattern. We have the pattern @racket[(cons x (cons y lst))]. The pattern that matches the @racket[cdr] of @racket[lst]
is another @racket[cons] pattern. The ability to nest patterns like this allows us to check deeply into data structures, which is something we couldn't
do with our @racket[bt-match] macro.

In the second example, we're matching against two values. A simple trick for doing this is creating a list with two values and matching the list. Here, we
use the @racket[list] pattern which can take an arbitrary number of sub-patterns. Intuitively, it matches values which are lists with length equal to the
number of sub-patterns and matches each element on its corresponding sub-pattern. In this case, since we're matching against the value @racket[(list xs ys)],
we use two sub-patterns. If they are both @racket[cons]es, we can @racket[cons] both elements to the zipped list by recurring. Otherwise, one of the lists
must be empty, so we just return the empty list.

Pattern matching is very powerful. We can check the shape of our data structures, reach deeply into them, check multiple cases,
and even perform case analysis on multiple values.

Let's do one more example just for fun. Let's take in a list of left/right steps representing a path in a binary tree and get the data at the
leaf specified by the path:

@repl[
(define (bt-get bt path)
  (match (list bt path)
    [(list (node bt _) (cons 'left path))
     (bt-get bt path)]
    [(list (node _ bt) (cons 'right path))
     (bt-get bt path)]
    [(list (leaf data) '())
     data]
    [(list _ '())
     (error 'bt-get "path too short")]
    [(list _ (cons _ _))
     (error 'bt-get "path too long")]))
(bt-get (node (leaf 1) (node (leaf 2) (leaf 3)))
        '(left))
(bt-get (node (leaf 1) (node (leaf 2) (leaf 3)))
        '(right left))
(bt-get (node (leaf 1) (node (leaf 2) (leaf 3)))
        '(right right))
(eval:error (bt-get (node (leaf 1) (node (leaf 2) (leaf 3)))
                    '(left left left)))
(eval:error (bt-get (node (leaf 1) (node (leaf 2) (leaf 3)))
                    '(right)))
]

We are using list patterns and tree patterns at the same time to do case analysis on two values of two different types.
Imagine how annoying this would've been to write without @racket[match]!

The first pattern covers the case where we should go left and the tree is a node. We don't care about the right sub-tree, so we ignore it
with an underscore pattern. We check for the symbol @racket['left] using a literal symbol pattern, which only matches if the value is equal
to the symbol. Numbers, strings, etc. can be matched with literal patterns in a similar way. Here, the pattern does most of the heavy lifting.
All we have to do is recur on the sub-tree and the rest of the path. The next case is the same, but for going to the right.
Next, we have the case where the path is empty and we're at a leaf. In this case, we return the data.

Any other case is an error. If the path is empty, then the tree must be a node. Otherwise, the leaf case would've run. This means the path was too short.
Similarly, if the list is non-empty, the tree must be a leaf, which means the path was too long.

You should always cover all possibilities when using @racket[match]. If none of the patterns match, we get an error. Really, we should always have an absolute fallback case where the whole pattern is
the underscore pattern. But for this function, we'll assume that the inputs are actually trees and paths.

Alright, hopefully I've sold you on the power of pattern matching by now if you weren't already familiar with it. Without further ado, let's implement it!

@section{Implementation}

Since we're going to be binding variables, we're going to need a macro. And this isn't going to be some simple @racket[define-syntax-rule], it's going to be
a full-on compiler! The main part of the implementation is going to be a compiler from patterns to condition checks, field accesses, and variable bindings.

To start off, let's write @racket[minimatch], which only handles a single case and only supports @racket[cons] and variable patterns.

To match a @racket[cons] pattern, we'll check that the value is a pair and recur, matching the sub-patterns on the @racket[car] and @racket[cdr] of the pair.
To match a variable pattern, we just bind it with a @racket[let].

@repl[
(define-syntax-rule
  (minimatch val [pat body ...])
  (let ([v val])
    (minimatch* v [pat (let () body ...)])))
(define-syntax minimatch*
  (syntax-rules (cons)
    [(_ val [(cons car-pat cdr-pat) body])
     (if (cons? val)
         (let ([car-val (car val)]
               [cdr-val (cdr val)])
           (minimatch* car-val
             [car-pat
              (minimatch* cdr-val
                [cdr-pat body])]))
         (error 'minimatch "match failed"))]
    [(_ val [var body])
     (let ([var val]) body)]))
(minimatch '(1 2)
   [(cons a (cons b c))
    (list a b c)])
]

The main macro just simplifies things for the helper macro which does most of the work. We make a local variable for the value and wrap up the multi-expression
body as a single expression. The helper expects that the value expression is a variable and the body is a single expression.

The helper macro is recursive on the pattern. To match a @racket[cons] pattern, we recur on the two sub-patterns.
To match two patterns, we match the first pattern, and in the body of that case, we match the second pattern, and that case gets the real body.
The rest is just structure checking and "field access". Since we are only supporting one case here, we just error if the pattern doesn't match.
In the full version, we'll go to the next case instead.

The variable pattern is super simple. To match a variable pattern, we just bind the value to the variable and run the body.

Let's see the expanded form of some examples:

@repl[
(require macro-debugger/expand)
(syntax->datum
 (expand-only
  #'(minimatch 1
      [a
       (add1 a)])
  (list #'minimatch #'minimatch*)))
(syntax->datum
 (expand-only
  #'(minimatch '(1)
      [(cons a b)
       (list a b)])
  (list #'minimatch #'minimatch*)))
(syntax->datum
 (expand-only
  #'(minimatch '(1 2)
      [(cons a (cons b c))
       (list a b c)])
  (list #'minimatch #'minimatch*)))
]

Since matching patterns like @racket[cons] ends up wrapping the body in more calls to @racket[minimatch*], we end up wrapping the body with @racket[if]s and
@racket[let]s. And we only run the body if the value matches the pattern and all the bindings are in scope.

Now, let's graduate to @racket[match] and handle multiple cases!

@repl[
(define-syntax match
  (syntax-rules ()
    [(match _) (lambda () (error 'match "all cases failed"))]
    [(match val [pat body ...] case ...)
     (let* ([v val]
            [on-fail (lambda () (match v case ...))])
       (match* v [pat (let () body ...)] on-fail))]))
(define-syntax match*
  (syntax-rules (cons)
    [(_ val [(cons car-pat cdr-pat) body] on-fail)
     (if (cons? val)
         (let ([car-val (car val)]
               [cdr-val (cdr val)])
           (match* car-val
             [car-pat
              (match* cdr-val
                [cdr-pat body]
                on-fail)]
             on-fail))
         (on-fail))]
    [(_ val [var body] on-fail)
     (let ([var val]) body)]))
(match '(1 2)
   [(cons a (cons b (cons c (cons d e))))
    42]
   [(cons a (cons b c))
    (list a b c)])
]

All we did was add an @racket[on-fail] argument to the helper macro. Instead of that default error, any time a pattern fails to match, it'll
just go to the next case if there is one and error if there isn't one.

Now, let's add some more pattern types:
