#lang scribble/manual

Title: Parsing Text the Racket Way
Date: 2024-08-24T17:02:21
Tags: racket, macros, dsls, tutorials

@require[scribble/example @for-label[racket syntax/parse]]
@(define eval (make-base-eval '(require racket)))
@(define-syntax-rule (repl body ...) (examples #:eval eval #:label #f body ...))

Have you ever needed to process some raw text and extract only parts of it? Most of the time, you can get by with some nasty regular expressions with groups, but sometimes the pattern that you're trying to process is too complicated for regular expressions. That's what parsers are good for, and they're also the first step in an interpreter/compiler!

In this post, we'll discover parsers and create a domain-specific language for creating parsers in Racket.

<!-- more -->

A parser is conceptually a function from a string to some other data structure. It reads in text and processes it into some data representation. For example, a JSON parser takes in JSON text and processes it into lists and maps of numbers, booleans, strings, and nulls. If you've ever tried to write something like a parser yourself, you probably used regular expressions. But regular expressions can quickly become unwieldy as the "language" of text you're processing becomes more complicated. And they're not re-usable. You can't easily reference one regular expression from another. Let's start by making our own version of regular expressions that are less compact, but easier to read and more re-usable.

Let's start out by thinking how this is going to work. We want a function that takes in a regular expression and a string and tells us whether the string matches the pattern specified by the regular expression. For example:

@racketblock[
(regexp-match? (seq (text "foo") (repeat (alt (text "bar") (text "baz"))))
               "foobarbazbarbar")
]

Here is the grammar for our little regular expression language:

@racketgrammar[#:literals (repeat alt text seq)
               regexp
               (repeat regexp)
               (alt regexp regexp)
               (seq regexp regexp)
               (text string-expr)]

@racket[repeat] matches zero or more occurrences of the sub-pattern, @racket[alt] matches if either sub-pattern matches, @racket[seq] matches if the first sub-pattern matches and the second sub-pattern matches the text @emph{after} the text matched by the first one, and @racket[text] matches the exact string passed to it.

In order to support @racket[seq] and @racket[repeat], we'll need some way to match a pattern on the beginning of a string and not expect it to match the entire string. Then, if it matched, we're going to need to know where the matched text ends so we can keep going on the rest of the text.

To keep track of where we are in the target string, we'll use an index:

@repl[
(struct stream [text index] #:transparent)
]

The text will be a string, and the index will be a natural number indicating where in the string we should start looking as we match text. As we go through the string, we'll increase the index.

Next, we have to figure out how we're going to represent regular expressions. What does a regular expression do? It should be able to take in a stream and either match text or fail. We'll represent matching the text by returning a new stream advanced to after the match, and failure as returning @racket[#f]. So a regular expression is a function which takes in a stream and either returns a stream or @racket[#f]. Let's start implementing!

@repl[
(define (text expected-str)
  (lambda (in)
    (match in
      [(stream text index)
       (define index^ (+ index (string-length expected-str)))
       (and (<= index^ (string-length text))
            (string=? expected-str (substring text index index^))
            (stream text index^))])))
((text "foo") (stream "foo" 0))
((text "foo") (stream "foobar" 0))
((text "foo") (stream "fo" 0))
((text "foo") (stream "" 0))
((text "foo") (stream "hello" 0))
((text "foo") (stream "aafooaa" 2))
]

We make sure the @emph{next} text in the stream (starting at @racket[index]) is @racket[expected-str] and if it is, we advance the stream past that text. Otherwise, we return @racket[#f]. We also make sure there is enough text left in the stream to even match the text to avoid an index out of bounds error.

Let's do @racket[seq] now:

@repl[
(define (seq re1 re2)
  (lambda (in)
    (define in^ (re1 in))
    (and in^ (re2 in^))))
((seq (text "foo") (text "bar")) (stream "foobar" 0))
((seq (text "foo") (text "bar")) (stream "foobarbaz" 0))
((seq (text "foo") (text "bar")) (stream "fooxxx" 0))
((seq (text "foo") (text "bar")) (stream "xxxbar" 0))
]

We run @racket[re1] on the initial input stream and if it succeeds, we run @racket[re2] on the advanced input stream.

Now @racket[alt]:

@repl[
(define (alt re1 re2)
  (lambda (in)
    (or (re1 in)
        (re2 in))))
((alt (text "foo") (text "bar")) (stream "foo" 0))
((alt (text "foo") (text "bar")) (stream "bar" 0))
((alt (text "foo") (text "bar")) (stream "hello" 0))
]

Pretty straightforward translation! We try the first pattern and if it fails, we try the second on the same stream, from the same index that we started with (since there is no mutation, it doesn't matter if @racket[re1] "advanced" the stream before failing).

Now, let's wrap it up with @racket[repeat]:

@repl[
(define (repeat re)
  (lambda (in)
    (define in^ (re in))
    (if in^
        ((repeat re) in^)
        in)))
((repeat (text "ha")) (stream "ha" 0))
((repeat (text "ha")) (stream "" 0))
((repeat (text "ha")) (stream "hahahahahahaha" 0))
((repeat (text "ha")) (stream "hahahahahahaha very funny" 0))
]

If @racket[re] succeeds, we recur. Otherwise, we succeed anyway with the original input stream. Remember, we succeed on zero or more matches, so if the sub-pattern fails, the @racket[repeat] still succeeds.

It's important to note that this is a greedy implementation of @racket[repeat] that will cause some patterns to fail when they shouldn't:

@repl[
((seq (repeat (text "a")) (text "a")) (stream "aaa" 0))
]

This should theoretically succeed with the @racket[(repeat (text "a"))] matching the first two @racket["a"]s and the second @racket[(text "a")] matching the third, but since the repetition succeeds greedily matching the whole string, the second text pattern fails since the stream ended. We could implement backtracking matching, but to keep things simple, we'll just be mindful of this behavior and continue on.

To provide our desired interface, let's implement @racket[regexp-match?]:

@repl[
(define (regexp-match? re s)
  (define in^ (re (stream s 0)))
  (and in^
       (= (string-length s) (stream-index in^))))
(regexp-match? (text "foo") "foo")
(regexp-match? (text "foo") "foobar")
]

We add that index condition to make sure the pattern matches the entire string, not just a prefix.

What we have right now is less powerful than regular expressions in some ways, like the fact that we can't use groups to extract pieces of text. We'll address that soon. However, what we have is more powerful than regular expressions in other ways. For example, we can create recursive regular expressions to match patterns that are impossible to match with typical regular expressions:

@repl[
(define balanced-parentheses
  (lambda (in)
    ((seq (text "(")
          (seq (alt balanced-parentheses (text ""))
               (text ")")))
     in)))
(regexp-match? balanced-parentheses "()")
(regexp-match? balanced-parentheses "(())")
(regexp-match? balanced-parentheses "(((())))")
(regexp-match? balanced-parentheses "(")
(regexp-match? balanced-parentheses "())")
]

That's impossible with typical non-recursive regular expressions. Also, that lambda may seem pointless, but if we don't do that, we won't be able to construct recursive regular expressions without going into an infinite loop. The lambda delays the recursion.

To avoid writing that lambda, we can make a macro:

@repl[
(define-syntax-rule (regexp re) (lambda (in) (re in)))
(define balanced-parentheses
  (regexp
    (seq (text "(")
         (seq (alt balanced-parentheses (text ""))
              (text ")")))))
]

We can also re-use regular expressions and create abstractions:

@repl[
(define (parenthesized re)
  (seq (text "(")
       (seq re
            (text ")"))))
(regexp-match? (parenthesized (text "foo")) "(foo)")
(define spaces (repeat (text " ")))
(define (sexpr atom)
  (regexp
    (alt atom
         (parenthesized (repeat (seq (sexpr atom) spaces))))))
(define number (alt (text "0")
                    (seq (text "1")
                         (repeat (alt (text "0") (text "1"))))))
(regexp-match? (sexpr number) "(1 (100 10 0) ())")
]

Nice! Using just these simple tools, you could make a JSON recognizer, or even a recognizer for most programming languages. I say recognizer instead of parser because all we get is a boolean, not structured data. For that, we'll need full-blown parsers.

A parser is going to be like a regular expression, except instead of just returning an input stream or @racket[#f], it can return a result with the input stream on success. It will still just return @racket[#f] on failure.

Let's define a grammar for parsers:

@racketgrammar[#:literals (repeat alt seq text bind =>)
               parser
               string-literal
               (text string-expr)
               (alt parser ...+)
               (seq parser ...+)
               (repeat parser)
               (bind id parser)
               (=> parser expr)
               racket-expr]

@racket[text] will return the matched text, string literals get converted into @racket[text] parsers, @racket[alt] will return the result of whichever parser succeeds, @racket[seq] will return a @racket[cons] pair containing each sub-parser result, @racket[repeat] will return a list of parse results from the sub-parser, @racket[bind] binds the result of the sub-parser to a variable and returns it, and @racket[=>] ignores the sub-parser result and returns the value of the sub-expression instead. It's usually used with @racket[bind] to combine results of sub-parsers. The @racket[racket-expr] case is for referencing defined parsers like @racket[spaces] and @racket[parenthesized].

Here is an example:

@racketblock[
(struct addition [left right] #:transparent)
(struct multiplication [left right] #:transparent)
(define expr (parser (alt number-expr addition-expr multiplication-expr)))
(define number-expr
  (parser (alt (=> (text "1") 1)
               (=> (text "0") 0))))
(define addition-expr
  (parser
    (=> (seq "(" (bind left expr) "+" (bind right expr) ")")
        (addition left right))))
(define multiplication-expr
  (parser
    (=> (seq "(" (bind left expr) "*" (bind right expr) ")")
        (multiplication left right))))
(parse expr "((1+1)*(1+1))")
]

In this example, we define a parser for a tiny arithmetic language. We define structs for an abstract syntax tree and parse strings into our structs.

Since we have @racket[bind], we're going to need macros. And the binding structure is pretty weird here. We're binding a variable deep inside an expression and using it outside. It's sort of like @racket[match]. We're going to need a full-on macro compiler. In fact, our compiler will look a lot like the compiler for @racket[match] that we made in @link["/blog/2023/10/understanding-and-implementing-pattern-matching.html"]{this post}.

Let's start by defining what we can as procedures and then add macros on top. Instead of returning either @racket[#f] or an input stream, we're going to return a new structure containing both an input stream and a parse result on success:

@repl[
(struct parse-result [in^ value] #:transparent)
]

We can define @racket[parse] and @racket[text] as procedures:

@repl[
(define (parse parser s)
  (define result (parser (stream s 0)))
  (match result
    [(parse-result (stream _ idx^) value)
     #:when (= (string-length s) idx^)
     value]
    [_ #f]))
(define (text expected-str)
  (lambda (in)
    (match in
      [(stream text index)
       (define index^ (+ index (string-length expected-str)))
       (and (<= index^ (string-length text))
            (string=? expected-str (substring text index index^))
            (parse-result (stream text index^)
                          expected-str))])))
(parse (text "foo") "foo")
(parse (text "foo") "fooooooo")
(parse (text "foo") "bar")
]

@racket[parse] is like @racket[regexp-match?], but we return the result of the parse instead of a boolean.

@racket[text] is like regexp @racket[text], except we return the matched string in a @racket[parse-result] with the resulting stream on success.

Unfortunately, we can't easily define runtime representations for our other parsers like @racket[seq]. Due to the nature of how we're going to do binding, we're going to inline them in our compiler.

Let's write our compiler top-down:

@repl[
(require (for-syntax syntax/parse))
(define-syntax-rule (parser p) (lambda (in) (compile-parse p in (lambda (in^ v) (parse-result in^ v)) (lambda () #f))))
(define-syntax compile-parse
  (syntax-parser
    [(_ p in:id on-success on-fail)
     (syntax-parse #'p
       #:datum-literals (seq alt repeat => bind)
       [s:string
        #'(compile-parse (text s) in on-success on-fail)]
       [(seq p ...)
        #'(compile-parse-seq (p ...) in on-success on-fail)]
       [(alt p ...)
        #'(compile-parse-alt (p ...) in on-success on-fail)]
       [(repeat p)
        #'(compile-parse-repeat p in on-success on-fail)]
       [(=> p e)
        #'(compile-parse-=> p e in on-success on-fail)]
       [(bind x p)
        #'(compile-parse-bind x p in on-success on-fail)]
       [e #'(call-with-parse-result (e in) on-success on-fail)])]))
(define (call-with-parse-result result on-success on-fail)
  (match result
    [(parse-result in^ value)
     (on-success in^ value)]
    [_ (on-fail)]))
]

The @racket[parser] form is like the @racket[regexp] form we defined before. It wraps the parser in a @racket[lambda], but it also invokes our compiler macro.

Our compiler takes in a parser's syntax @racket[p], an identifier that the input stream is bound to, @racket[in], syntax for an @racket[on-success] procedure that takes in the resulting input stream and the parse result's value, and syntax for an @racket[on-fail] procedure which takes in zero arguments. Like our @racket[match] compiler we made in @link["/blog/2023/10/understanding-and-implementing-pattern-matching.html"]{another post}, this compiler uses continuation-passing style with those @racket[on-success] and @racket[on-fail] procedures.

For each case, we defer to sub-compilers that we'll write in a moment, except for @racket[text] and the parser reference case, which we've inlined. The @racket[text] case just defers to runtime operations and the parser reference case treats the parser syntax as a Racket expression that produces a parser, like @racket[parenthesized] or @racket[spaces].

Now let's start defining these sub-compilers:

@repl[
(define-syntax compile-parse-seq
  (syntax-rules ()
    [(_ () in on-success _) (on-success in (list))]
    [(_ (p0 p ...) in on-success on-fail)
     (compile-parse p0 in
       (lambda (in^ v)
         (compile-parse (seq p ...) in^
           (lambda (in^^ vs)
             (on-success in^^ (cons v vs)))
           on-fail))
       on-fail)]))
]

To parse an empty @racket[seq], we simply succeed with an empty list. Otherwise, we run the first parser and @racket[cons] its result ot the results of the rest of the @racket[seq]'s results. The rest is just threading around the input stream.

Unlike our regular expression @racket[seq], this one can take zero or more sub-parsers, not just two.

@repl[
(define-syntax compile-parse-alt
  (syntax-rules ()
    [(_ () _ _ on-fail) (on-fail)]
    [(_ (p0 p ...) in on-success on-fail)
     (compile-parse p0 in
       on-success
       (lambda () (compile-parse (alt p ...) in on-success on-fail)))]))
]

An empty @racket[alt] fails. Otherwise, we just try each sub-parser.

@repl[
(define-syntax-rule
  (compile-parse-=> p e in on-success on-fail)
  (compile-parse p in (lambda (in^ _) (on-success in^ e)) on-fail))
]

For @racket[=>], which is called a semantic action, we ignore the result of the sub-parse and just return @racket[e] instead.

@repl[
(define-syntax-rule
  (compile-parse-bind x p in on-success on-fail)
  (compile-parse p in
    (lambda (in^ v)
      (let ([x v])
        (on-success in^ v)))
    on-fail))
]

For @racket[bind], we bind the result to the variable name on success.

Here is an example expansion of a semantic action and binding so we can see how they fit together:

@racketblock[
(parser (=> (bind x "foo")
            (string-upcase x)))
]
becomes something like
@racketblock[
(lambda (in)
  (call-with-parse-result
   (text-rt "foo")
   (lambda (in^ v)
     (let ([x v])
       ((lambda (in^^ _)
          ((lambda (in^^^ v) (parse-result in^^^ v))
           in^^
           (string-upcase x)))
        in^
        v)))
   (lambda () #f)))
]

Our compiler creates nested @racket[on-success] lambdas, which is where binding happens. The resulting syntax is almost inside-out from the original, having the innermost sub-parsers on the outside executing first, as they should, and the outermost parsers on the inside executing last, on the results of the sub-parsers. That's how bindings inside the parser are usable in outer parts of the parser.

Next up is @racket[repeat], which is a little tricky because of binding. Consider this example:

@racketblock[
(define number-expr
  (parser (alt (=> (text "1") 1)
               (=> (text "0") 0))))
(define array-expr
  (parser (=> (seq "[" (repeat (seq (bind n number-expr) ",")) "]")
              n)))
(parse array-expr "[1,0,1,]")
]

The @racket[bind] is in the @racket[repeat], but its variable is used outside of the @racket[repeat]. What should @racket[n] be bound to outside of the @racket[repeat]? This is like the syntax pattern

@racketblock[(let ([x rhs] ...) body)]

In that pattern, @racket[x] gets bound to the list of all variables bound in the let. We'll do something similar, so outside of the @racket[repeat], any variable that got bound inside will end up bound to the list of values it was bound to internally. So the result of that @racket[parse] should be @racket[(list 1 0 1)].

@repl[
(require (for-syntax syntax/parse))
(begin-for-syntax
  (define (parser-bound-vars p)
    (syntax-parse p
      #:datum-literals (seq alt repeat => bind)
      [((~or seq alt repeat) p ...) (apply append (cons (list) (map parser-bound-vars (attribute p))))]
      [(=> p _) (parser-bound-vars #'p)]
      [(bind x p) (cons #'x (parser-bound-vars #'p))]
      [_ (list)])))
(define-syntax compile-parse-repeat
  (syntax-parser
    [(_ p in on-success on-fail)
     (define/syntax-parse (v ...) (parser-bound-vars #'p))
     (define/syntax-parse (iter-v ...) (generate-temporaries (attribute v)))
     (define/syntax-parse p^ #'(=> p (begin (set! iter-v (cons v iter-v))
                                            ...)))
     #'(let ([iter-v (list)] ...)
         (match-define (parse-result overall-in overall-result)
           (let loop ([in^ in])
             (compile-parse p^ in^
               (lambda (in^^ value)
                 (call-with-parse-result (loop in^^)
                   (lambda (in^^^ values) (parse-result in^^^ (cons value values)))
                   (lambda () (assert-unreachable))))
               (lambda () (parse-result in^ (list))))))
         (let ([v (reverse iter-v)] ...)
           (on-success overall-in overall-result)))]))
]

We loop, running @racket[p] as many times as we can. Each time, we take all of the values bound inside @racket[p] and @racket[cons] them to a list that contains the values of that variable for each parse of @racket[p].

For example:

In
@racketblock[
(repeat (seq (bind n number-expr) ","))
]
the sub-parser becomes
@racketblock[
(=> (seq (bind n number-expr) ",")
    (begin (set! iter-n (cons n iter-n))))
]
and we run that parser for each iteration after initializing @racket[iter-n] to an empty list before we start looping.

In the end, we reverse the lists because we built them up backwards, and then bind each variable to its corresponding list for outer use.

Now let's parse some strings!

@repl[
(struct addition [left right] #:transparent)
(struct multiplication [left right] #:transparent)
(define expr (parser (alt number-expr addition-expr multiplication-expr)))
(define number-expr
  (parser (alt (=> (text "1") 1)
               (=> (text "0") 0))))
(define addition-expr
  (parser
    (=> (seq "(" (bind left expr) "+" (bind right expr) ")")
        (addition left right))))
(define multiplication-expr
  (parser
    (=> (seq "(" (bind left expr) "*" (bind right expr) ")")
        (multiplication left right))))
(parse expr "((0+1)*(1+0))")

(define array-expr
  (parser (=> (seq "[" (repeat (seq (bind n number-expr) ",")) "]")
              n)))
(parse array-expr "[1,0,1,]")
]

It is nice that we can define our own parsers and compose them, but there is a limitation with higher order parsers. For example:

@repl[
(define (parenthesized p)
  (parser (seq "(" p ")")))
(eval:error
 (parse (parser (=> (parenthesized (parser (bind x "foo")))
                    x))
        "(foo)"))
]

Since @racket[parenthesized] is just a procedure call, our compiler can't go inside of it and see if variables are bound, so @racket[x] winds up unbound.

If instead, @racket[parenthesized] was some kind of parser macro, then it would work. But we'd need our own custom DSL expander to support parser macros. There is a tool I'm helping develop that will make it easy to do this, but that's a post for another day.

Either way, what we've just made is very powerful. We can now write parsers for JSON and even most programming languages! And our parsers are composable, declarative, and have the full power of Racket. We created custom syntax for a parsing DSL that inter-operates with normal Racket code, which is something that isn't possible in most programming languages. This is a task where language-oriented programming comes in very handy.
