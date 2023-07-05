#lang scribble/manual

Title: Creating a Proof Checker
Date: 2023-07-03T22:57:22
Tags: DRAFT, racket, math, projects, tutorials

@require[
  scribble/example
  @for-label[
    racket
  ]
]

When you're proving something in math, how do you actually know if you're doing it right? What if you make a mistake? What if you gloss over proving something which is "obviously true" that turns out to be false? One way is to have a computer check your proof, and we're going to build a proof-checker together!

<!-- more -->

@table-of-contents[]

Proofs are essential to math. We start out with some facts we assume to be true called axioms. From those axioms, we can deduce other facts. If a fact is proven to be true given the axioms, it is called a theorem. Working in such a system, we can use theorems to prove other theorems. In this way, math builds on itself, using reasoning to accumulate more and more facts that follow from those initial assumptions. But this process relies on a few things to make sense: Your axioms and rules of reasoning must be "sensible", and you must apply them correctly in a proof. In this blog post, we will create a language for stating mathematical facts, a "sensible" set of rules for reasoning and a system that checks the correctness of proofs using those rules. We will be able to throw in whatever axioms we want and explore what facts we can prove from them.

For this post, you actually won't need to know much math. It would help to be sort of familiar with proofs, but we're actually going to be building math from the ground up, so you won't need much to start with. For the implementation, we will be using the Racket programming language, but you don't need to know Racket. I'll explain everything as we go. A general familiarity with programming will be enough.

@; TODO verify prereqs at the end

@section{First-order Logic}

In order to state facts, we need a language to write those facts in. For example, let's consider a statement written in English:

"All men are mortals. Socrates is a man. Therefore Socrates is mortal."

What exactly is the relationship between these sentences? What do "is" and "are" mean? You can look up the definitions for "is" and "are" in a dictionary, but those definitions are in terms of other imprecise english words. What we want are some well-defined connectives for building statements and relating objects. To this end, we will use the language of first-order logic.

Here is our example written in this language:

\[
(\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
\]

This statements also uses the language of set theory, which we'll get into later. Let's go over all the symbols and their meaning:

\(\forall\) means "for all", \(\in\) means "is an element of", \(\rightarrow\) means "implies", and \(\wedge\) means "and". \(\forall\) is what is called a quantifier. Specifically, it is for universal quantification, which allows us to state that some property is true for all things. The rest of the symbols are operators and logical connectives. \(\in\) is from set theory and \(x \in S\) means \(x\) is an element of the set \(S\). \(p \rightarrow q\) can be read as "if p, then q". This statement is true when \(p\) being true implies that \(q\) is true. \(p \wedge q\) is read "p and q" and is true when both \(p\) and \(q\) are true. Putting it all together, we have "If, for all things x, x being a member of the set of men implies that x is a member of the set of mortals and socrates is in the set of men, then socrates is in the set of mortals." We encode "is" with set membership and we have quantifiers and logical connectives for chaining facts together in precise and meaningful ways.

To determine whether this statement is true, we'll need to define how these quantifiers and connectives work. But first, let's fully describe our language of statements:

A formula \(p,q\) is one of

A variable

\[x\]

A universal quantification


\[
\forall x (p)
\]

An existential quantification

\[
\exists x (p)
\]

An implication (if-then)

\[
p \rightarrow q
\]

A conjunction (and)

\[
p \wedge q
\]

A disjunction (inclusive or)

\[
p \vee q
\]

A negation (not)

\[
\neg p
\]

An operation. Like \(x \in y\) for set membership.

A formula in first-order logic represents a mathematical statement of a fact that may or may not be true. I'll be using "formula" and "statement" somewhat interchangably, but remember that a statement is the idea of a fact, and a formula is a way of writing it down in our language. This is a subtle, but important difference.

This is all very abstract, so let's look at some examples to get a feel for this language.

Here is an example using existential quantification:

\[\forall x (\exists S (x \in S))\]

This means "For all x, there exists S such that x is an element of S". This is saying that for any object, there is a set containing that object. This is not saying that there is only one such set, just that there is at least one.

Here is another example:

\[\forall x (\forall S (x \in S \vee \neg (x \in S)))\]

This means "For all x, for all S, x is an element of S or x is not an element of S".

Let's go through each type of formula and think about what these statements mean:

Universal quantification is used to make a statement about all things. \(\forall x (p)\) means "for all x, the statement p is true", where \(p\) is some statement about \(x\). Here, \(x\) is a variable in our language, and \(p\) is a meta-variable representing some formula/statement in our language. This is a subtle distinction. When you see something like \(x,y,z,S\), that means a variable in our language, and when you see \(p,q,r\), that means some formula, not literally the variable \(p\). So \(x\) is literally the formula \(x\), and \(p\) is a meta-variable representing some formula in our language.

Existential quantification is used to say that there exists an object for which some statement about it is true. \(\exists x (p)\) means "there exists an x such that p is true", where \(p\) says something about \(x\).

Implication has a few uses. One is to make assumptions. \(p \rightarrow q\) means that if you assume that \(p\) is true, then \(q\) must be true. This sounds like how we use axioms. Another use is to state implications directly. In the Socrates example, we want to say "All men are mortals". To state that fact in our language, we said \(\forall person (person \in men \rightarrow person \in mortals)\), meaning being a man implies being a mortal. This pattern of an implication in a forall is very common. Often times, we don't want to make a direct statement about all things, but rather all things with some property. \(\forall x (p \rightarrow q)\) means "for all x where p is true, q is true."

Conjunction (and) is used to state that two facts are both true. \(p \wedge q\) is true when both \(p\) is true and \(q\) is true.

Disjunction (inclusive or) is used to state that one fact or another fact are true, or both. \(p \vee q\) is true when \(p\) is true, \(q\) is true, or they're both true.

A negation (not) is used to state that a fact is not true. \(\neg p\) is true when \(p\) is not true.

We have been using set membership \(\in\) in our examples, but we haven't defined it. Operators like \(\in\) aren't part of the fundamental language of first-order logic, and the rules of logic are independent from the definitions of operators. We'll see how they fit in later. I only included statements about sets in our examples so we can have something concrete to state facts about. Otherwise, it's very abstract and hard to think about.

Let's think about what the variables in our statements really mean. In examples using set membership, variables represented sets and elements of sets. But we can also make statements like this:

\[\forall x (x \rightarrow x)\]

This means that "for all x, if x is true, then x is true". This is a statement about statements, rather than a statement about sets. And it feels like it should be true, so it's not nonsense. In fact, this is a completely valid statement. But if variables can represent statements and also objects like sets, then we can write nonsense like

\[
\forall x (\forall S (x \in (\neg S)))
\]

This is treating \(S\) as a statement and as a set, which is nonsense. Saying that an object is an element of a statement is meaningless.

It's actually fine that we can write nonsense like this. We just have to make sure that our axioms and rules for reasoning don't allow us to prove nonsensical statements like that to be true. Or we can create a system in which statements like this actually have some meaning, which is also fine. We just have to keep in mind that variables can represent statements as well as other mathematical objects like sets.

@subsection{True and False}

How do we express "true" and "false" in our language? To answer that, let's ask another question: What do "true" and "false" mean? Well, we have to decide. To avoid confusion between the statements representing "true" and "false" and statements being proven true and proven false, I'm going to write the statement representing "true" as \(T\) and the statement representing "false" as \(F\). These are meta-variables representing formulas, like \(p\) and \(q\).

If \(T\) was a statement in our language, what properties would we want it to have? That statement should always be true. Similarly, it should be impossible to prove that the statement \(F\) is true, unless you assume that it is true for some reason. For example, \(F \rightarrow F\) is true because we are assuming that \(F\) is true. And if \(F\) is true, then \(F\) is true. We can do that with any statement, even nonsensical statements. If you assume that a nonsensical statement is true, then it is true. Another way we can say this property of \(F\) is that the statement \(\neg F\) should always be true.

Alright, what statements can we make in our language that have these properties? Let's start with \(F\). What statement cannot be proven? One such statement is

\[
x \wedge \neg x
\]

Treating \(x\) as a statement, this is saying that \(x\) is true and false. \(x\) cannot be both true and false at the same time, so this statement is false. Another example is

\[
\forall x (x)
\]

This is saying that all statements are true. Not only that, but it is saying that all things are true statements. This statement is obviously false. We can use any false statement to disprove it. What's nice about this statement is that it has no "free variables". We'll get into more detail about this later, but for now, let's think about this: If we used \(x \wedge \neg x\) to represent "false", we might want to use that statement in another statement where \(x\) already has some meaning. For example, we might be making a statement about some set \(x\) and we want to use the statement representing "false" in that statement about \(x\). \(x \wedge \neg x\) is treating \(x\) as a statement even though it is a set. That would be nonsense. We could just use a different version of "false" where we write \(y \wedge \neg y\) instead where \(y\) has no current meaning, but it would be nice if we just had one statement that we could drop in anywhere and have no "collisions" with what the rest of the statement is saying. In the second example, the quantifier \(\forall x\) is saying "for all x", so it is creating a new meaning for \(x\) in the body of the quantification. Don't worry if this "free variable" business doesn't make sense yet. We'll make all this more concrete later. For now, let's just avoid the weirdness that comes with using \(x\) freely like in the first example and prefer \(\forall x (x)\).

We will represent "false" with the formula \(\forall x (x)\). In other words we choose \(\forall x (x)\) for \(F\) .

Ok, now for \(T\). What statement is always true?

Well, \(T\) is the "opposite" of \(F\) and the "opposite" of the first candidate for \(F\) is

\[
x \vee \neg x
\]

Treating \(x\) as a statement, this is saying that \(x\) is true or false. This is always true for any statement. But again, what if this is used in a statement where \(x\) is a set or something? Nonsense! Ok, since \(T\) is the opposite of \(F\), what is the opposite of \(\forall x (x)\)? That'd be

\[
\exists x (x)
\]

This is saying that there exists a statement \(x\) that is true. This statement is true, and it has no free variables, so no "collisions" and nonsense.

This idea of an "opposite" has a name: the dual. Existential quantification is dual to universal quantification and disjunction (or) is dual to conjunction (and). Specifically,

\[
\neg (p \vee q)
\]
is equivalent to
\[\neg p \wedge \neg q\]

and

\[
\neg (p \wedge q)
\]
is equivalent to
\[\neg p \vee \neg q\]

These are known as De Morgan's laws.

The first equivalence means that for the the disjunction of two statements to be false, they must both be false.

The second equivalence means that for the conjunction of two statements to be false, one or both must be false.

For quantification,

\[\neg \forall x (p)\]

is equivalent to

\[\exists x (\neg p)\]

and

\[\neg \exists x (p)\]

is equivalent to

\[\forall x (\neg p)\]


Let's think about the first equivalence. A forall means that the statement \(p\) about \(x\) is true for all \(x\). This is false when there exists some \(x\) for which the statement \(p\) about \(x\) is false.

Now the second equivalence. An existential means that there exists some \(x\) for which the statement \(p\) about \(x\) is true. This is false when, for all things \(x\), the statement \(p\) about \(x\) is false.

The dual of a statement is its negation.


For anything you say about a universal quantification or a conjunction, there exists a dual statement that is often interesting to think about too.

Are \(T\) and \(F\) duals of each other? No. \(\forall x (x)\) is not actually the negation of \(\exists x (x)\). That would be \(\forall x (\neg x)\), which means that all statements are false. This statement is also false, so we could use it to represent "false". But, as we'll see, \(\forall x (x)\) is more convenient to work with and it is actually equivalent to \(\forall x (\neg x)\).

There are two subtle, weird details about \(\exists x (x)\) and \(\forall x (x)\). \(\exists x (x)\) is true as long as there exists a statement that is true. But what if there were no true statements? Then it would be impossible to prove that this statement is true. What about the dual of this fact? \(\forall x (x)\) is true if all statements are true. Equivalently, it is false if there exists a statement that is false. But what if there were no false statements? Then it would be impossible to prove that this statement is false. This is concerning, but since we have connectives that allow us to make true and false statements in our language, this is not actually a problem.

@subsection{Rules for Reasoning}

We now have a language for stating mathematical facts. Now, we need a set of rules for proving that statements are true. These are our rules for reasoning.

@;To start out with creating rules for reasoning, we're going to temporarily forget about operations like set membership and focus on pure logical statements. This means we'll be working exclusively with statements about statements, not statements about objects like sets. These statements about statements are totally valid, meaningful, and important, but they are abstract and can be hard to reason about.

In order to prove anything interesting, we need to start out with some assumptions. Throughout a proof, we'll have some collection of assumptions which we'll use to prove some statements. We will represent this collection of assumptions with a context, often denoted with the greek letter \(\Gamma\), pronounced "gamma". A context is a list of formulas which are assumed to be true statements. For example, in the socrates example, we assumed that all men are mortals and that socrates is a man. We could represent these assumptions as a context like this:

\[
\Gamma = \forall x (x \in men \rightarrow x \in mortals), socrates \in men
\]

In this case, \(\Gamma\) is a list of two assumptions. If we want to make a statement under those assumptions, we'd write it like this:

\[
\forall x (x \in men \rightarrow x \in mortals), socrates \in men \vdash socrates \in mortals
\]

The turnstile symbol \(\vdash\) separates the assumptions from the statement you're making. In general,

\[
\Gamma \vdash p
\]

means under the assumptions in the context \(\Gamma\), \(p\) is true. This is called a judgement. Rather than proving statements directly, we will be proving judgements. For a theorem, \(\Gamma\) would contain your axioms and \(p\) would be the statement of your theorem. Proving a theorem is proving your statement to be true assuming that the axioms are true.

Let's think back to our original statement of the socrates example:

\[
(\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
\]

One of the uses of implication is to encode the idea of an assumption. Isn't this the same as our judgement? Yes! In general, the judgement

\[
p,q,\ldots \vdash r
\]

is equivalent to the statement

\[
p \wedge q \wedge \ldots \rightarrow r
\]

We will represent this fact with two rules. One for proving an implication:

\[
\frac{
\Gamma, p \vdash q
}{
\Gamma \vdash p \rightarrow q
} ({\rightarrow} R)
\]

and one for using a conjunction:

\[
\frac{
\Gamma, p, q \vdash r
}{
\Gamma, p \wedge q \vdash r
} ({\wedge} L)
\]

Let's start out by talking about this notation for rules. We have two judgements separated by a line called an inference line, and we have the name of the rule to the right of the whole thing, like \(({\rightarrow} R)\). The meaning of a rule written this way is, in order to prove the bottom judgement, you must prove the top judgement. I find it easiest to read these rules starting at the bottom and then the top. For our implication rule \(({\rightarrow} R)\), we are saying that in order to prove that an implication \(p \rightarrow q\) is true using some assumptions \(\Gamma\), it suffices to prove that \(q\) is true if we add \(p\) to our list of assumptions. This is how we use implication to represent assumptions. It is literally baked in to our rules for reasoning.

Now let's talk about that second rule, \(({\wedge} L)\). This is saying that if we assume that a conjunction \(p \wedge q\) is true, we can assume that both statements are true and add both statements to our list of assumptions.

A proof in this system will consist of a sequence of applications of rules until we reach a judgement that requires no sub-proofs to prove. We'll see how that works soon. This is all pretty abstract, so let's bring it back to our socrates example and sketch out a proof of that statement:

We start out with this judgement:

\[
\vdash (\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
\]

Notice that there is nothing to the left of the turnstile. This means we are starting out with no assumptions. Now, to start proving this statement, we can use our rules:

\[
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\cdots
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals), socrates \in men \vdash socrates \in mortals
} (?)
} {
\displaystyle
\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men \vdash socrates \in mortals
} ({\wedge} L)
}{
\displaystyle
\vdash (\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
} ({\rightarrow} R)
\]

Remember, we read bottom up. We start out with no assumptions. We must prove an implication, so we use \(({\rightarrow} R)\). Then, we have a conjunction in our assumptions, so we use \(({\wedge} L)\) to add both statements to our assumptions. We don't know what to do next, so we'll leave it for now.

I chose to name these rules \(({\rightarrow} R)\) and \(({\wedge} L)\) for a reason. You use \(({\rightarrow} R)\) to prove that an implication is true, and you use \(({\wedge} L)\) to use a conjunction in your assumptions. In fact, for all of our different types of formulas, there is an \(L\) rule for using it as an assumption and an \(R\) rule for proving that the statement is true. To make some more progress in our Socrates proof, let's define how we can use a universal quantification.

If we assume that a universal quantification \(\forall x (p)\) is true, that means the statement \(p\) is true for any thing \(x\). In our example, we assume

\[
\forall x (x \in men \rightarrow x \in mortals)
\]

We also assume

\[
socrates \in men
\]

If we assume \(\forall x (x \in men \rightarrow x \in mortals)\), then we are assuming that this is true for all things. So this should also be true for socrates! In other words, we should be able to assume that

\[
socrates \in men \rightarrow socrates \in mortals
\]

What we just did is substitute \(socrates\) for \(x\) in the body of the quantification. In general, if we assume a universal quantification \(\forall x (p)\) to be true, we should be able to substitute anything for \(x\) in \(p\) and assume that to be true. This is the \(({\forall} L)\) rule:

\[
\frac{
\Gamma, p[t \mathbin{/} x] \vdash q
} {
\Gamma, \forall x (p) \vdash q
} ({\forall} L)
\]

where \(p[t \mathbin{/} x]\) denotes substituting the formula \(t\) in place of the variable \(x\) in \(p\). Here, \(t\) can be any formula, not just a variable.

This is sometimes referred to as "instantiating" the universal quantification with a particular "term" \(t\).

Cool! Let's use this in our Socrates proof:

\[
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\cdots
}{
\displaystyle
socrates \in men \rightarrow socrates \in mortals, socrates \in men \vdash socrates \in mortals
} (?)
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals), socrates \in men \vdash socrates \in mortals
} ({\forall} L)
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men \vdash socrates \in mortals
} ({\wedge} L)
}{
\displaystyle
\vdash (\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
} ({\rightarrow} R)
\]

Our \(({\forall} L)\) rule is written as if the universal quantification has to be the last element of the context, but order of assumptions doesn't matter, so it can be anywhere in the context for the rule to be applicable. We'll make this fact concrete and formal later.

We're one step closer! Now we have an implication in our context of assumptions. How do we use that?

Remember, the direct interpretation of the implication \(p \rightarrow q\) being true is that \(p\) being true implies that \(q\) is true. In other words, if we assume that \(p \rightarrow q\) is true and we prove that \(p\) is true, then we should be able to assume that \(q\) is true. This is the \(({\rightarrow L})\) rule:

\[
\frac{
\Gamma \vdash p \qquad \Gamma, q \vdash r
}{
\Gamma, p \rightarrow q \vdash r
} ({\rightarrow} L)
\]

This means that when we assume an implication \(p \rightarrow q \) and we're trying to prove that some statement \(r\) is true, if we can prove the antecedent \(p\) is true, then we can assume the consequent \(q\) is true and use it to prove \(r\). This is an example of a rule which requires two subproofs instead of just one. We have to prove \(p\) before we can assume \(q\).

Let's use it in our proof:

\[
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{\cdots}{socrates \in men \vdash socrates \in men} (?) \qquad \frac{\cdots}{socrates \in mortals, socrates \in men \vdash socrates \in mortals} (?)
}{
\displaystyle
socrates \in men \rightarrow socrates \in mortals, socrates \in men \vdash socrates \in mortals
} ({\rightarrow} L)
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals), socrates \in men \vdash socrates \in mortals
} ({\forall} L)
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men \vdash socrates \in mortals
} ({\wedge} L)
}{
\displaystyle
\vdash (\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
} ({\rightarrow} R)
\]

We're almost done!

For our remaining subproofs, the statement we're trying to prove is actually in the context of assumptions! If we assume that a statement is true, we don't need to do any more work to prove that it is true. This is the \((I)\) rule.

\[
\frac{}{\Gamma,p \vdash p} (I)
\]

Notice that there are no subproofs. In fact, all valid proofs will end with rules like this one, which have no subproofs. In this way, a proof is a tree of inferences, where each node is a judgement and an application of a rule, and its children are the sub-proofs required by the rule. The leaves of this tree are applications of rules like \((I)\) which require no subproofs.

Let's finish up our proof:

\[
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{}{socrates \in men \vdash socrates \in men} (I) \qquad \frac{}{socrates \in mortals, socrates \in men \vdash socrates \in mortals} (I)
}{
\displaystyle
socrates \in men \rightarrow socrates \in mortals, socrates \in men \vdash socrates \in mortals
} ({\rightarrow} L)
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals), socrates \in men \vdash socrates \in mortals
} ({\forall} L)
}{
\displaystyle
\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men \vdash socrates \in mortals
} ({\wedge} L)
}{
\displaystyle
\vdash (\forall x (x \in men \rightarrow x \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
} ({\rightarrow} R)
\]

We did it! Now we know for sure that Socrates is mortal! That is, as long as our assumptions are true and our rules for reasoning make sense.

We started out with an imprecise english statement of a fact. Then, we translated the statement into a precise mathematical language. Finally, we have proven that the statement is true using a set of simple rules that make intuitive sense.

Now, let's write down the rules for all the types of formulas:

We already figured out \(({\forall} L)\):

\[
\frac{
\Gamma, p[t \mathbin{/} x] \vdash q
} {
\Gamma, \forall x (p) \vdash q
} ({\forall} L)
\]

What about \(({\forall} R)\)? How do we prove that something is true about all things? In order to do that, we can instantiate the forall with some new variable that has no meaning and no assumptions about it. Here's what this looks like:

\[
\frac{
\Gamma \vdash p[y \mathbin{/} x]
}{
\Gamma \vdash \forall x (p)
} ({\forall} R)
\]

We must add the restriction that \(y\) must not be mentioned anywhere in our context of assumptions \(\Gamma\) or our statement \(\forall x (p)\). More precisely, \(y\) must not occur free in \(\Gamma\) or \(\forall x (p)\). This restriction is necessary because if we are proving that \(p\) is true for all \(x\), we must be able to prove \(p[y \mathbin{/} x]\) without knowing or assuming anything about \(y\).

For example,

\[
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
}{
\displaystyle
y \vdash y
} (I)
}{
\displaystyle
\vdash y \rightarrow y
} ({\rightarrow} R)
}{
\displaystyle
\vdash \forall x (x \rightarrow x)
} ({\forall} R)
\]

Now let's do \({\exists} L\). If we assume that \(\exists x (p)\) is true, then we know that there exists at least one thing \(x\) such that \(p\) is true. How can we use this assumption? We can instantiate the quantifier! We can introduce some new variable \(y\) representing the thing that exists, and add an assumption \(p[y \mathbin{/} x]\) stating that the statement \(p\) is true for \(y\). Similar to \(({\forall} R)\), \(y\) must not occur free in the context \(\Gamma\) or \(\exists x (p)\). All we are allowed to assume about \(y\) is that \(p[y \mathbin{/} x]\) is true. That's all we know about it.

\[
\frac{
\Gamma, p[y \mathbin{/} x] \vdash q
}{
\Gamma, \exists x (p) \vdash q
} ({\exists} L)
\]

This rule is dual to \(({\forall} R)\). It's the same thing, except everything is happening to the left of the turnstile instead of the right. Proving a \(\forall\) is dual to using an \(\exists\). Pretty cool!

Unsurprisingly, the rule \(({\exists} R)\) is dual to \(({\forall} L)\):

\[
\frac{
\Gamma \vdash p[t \mathbin{/} x]
}{
\Gamma \vdash \exists x (p)
} ({\exists} R)
\]

In order to prove an existential \(\exists x (p)\), you must substitute some term \(t\) for \(x\) in \(p\) and prove that \(t\) makes the statement true. In other words, to prove that something exists with a property, you give an example and prove that it has the property! And for the duality, proving an \(\exists\) is dual to using a \(\forall\).

We figured out \({\wedge} L\). What about \(({\wedge} R)\)? How do we prove \(p \wedge q\)? We prove \(p\) and we prove \(q\)!

\[
\frac{
\Gamma \vdash p \qquad \Gamma \vdash q
}{
\Gamma \vdash p \wedge q
} ({\wedge} R)
\]

Makes sense. Unsurprisingly, the disjuncion rules are dual to these conjunction rules:

\[
\frac{
\Gamma,p \vdash r \qquad \Gamma,q \vdash r
}{
\Gamma,p \vee q \vdash r
} ({\vee} L)
\]

This one is a little confusing on first glance. Let's think about what this means: We assume \(p \vee q\) is true. So we assume that either \(p\) is true, \(q\) is true, or they're both true. If we want to use this assumption, we need to handle each possible case (the case where they're both true is implicitly handled). Thus, we need a separate proof for each case.

\(({\vee} R)\) actually has two rules:

\[
\frac{
\Gamma \vdash p
}{
\Gamma \vdash p \vee q
} ({\vee} R1)
\]

\[
\frac{
\Gamma \vdash q
}{
\Gamma \vdash p \vee q
} ({\vee} R2)
\]

Since a disjunction is true when either statement is true, proving a disjunction only requires proving one of the statements.

Why isn't this dual to \(({\wedge} L)\)? It could have been. We could have written that as two rules as well:

\[
\frac{
\Gamma, p, \vdash r
}{
\Gamma, p \wedge q \vdash r
} ({\wedge} L1)
\]

\[
\frac{
\Gamma, q, \vdash r
}{
\Gamma, p \wedge q \vdash r
} ({\wedge} L2)
\]

But, with another rule we'll introduce later which allows us to duplicate assumptions, we'll see that \(({\wedge} L)\) is equivalent to applying that duplication rule, \(({\wedge} L1)\), and then \(({\wedge} L2)\). So we might as well always do both.

We already have both rules for implication.

All that's left is negation. Instead of thinking about \(\neg p\) directly, let's think about an equivalent form, \(p \rightarrow \forall x (x)\). To convince ourselves that these two forms are the same, let's think about the case when \(p\) is true. This implies \(\forall x (x)\), which is false. So when \(p\) is true, \(p \rightarrow \forall x (x)\) is false. This is just like with \(\neg p\). So far so good. When \(p\) is false, we essentially have \(\forall x (x) \rightarrow \forall x (x)\), which is true since every statement implies itself. So when \(p\) is false, \(p \rightarrow \forall x (x)\) is true. This is just like \(\neg p\). This is by no means a rigorous proof, but hopefully it convinces us that the two formulas are equal. If you want a more rigorous proof, you can use a truth table. Or, since we're making the rules here, we can just define \(\neg p\) to be \(p \rightarrow \forall x (x)\) and be happy that it fits our intuition for how negation should behave.

Anyway, how can we use the implication \(p \rightarrow \forall x (x)\) as an assumption? If we use \(({\rightarrow} L)\) and prove that \(p\) is true, then we can use \(({\forall}L)\) to assume that \(\forall x (x)\) is true. Then, we can use \(({\forall} L)\) to prove anything! In other words, if we reach a contradiction, all bets are off. This gives us the rule

\[
\frac{
\Gamma \vdash p
}{
\Gamma, \neg p \vdash q
} ({\neg} L)
\]

And how do we prove a negation \(\neg p\)? If that's the same as proving \(p \rightarrow \forall x (x)\), applying \(({\rightarrow} R)\), we get \(p\) as an assumption and we have to prove \(forall x (x)\). This makes sense because the only way to prove \(\forall x (x)\) is if we reach a contradiction that allows us to prove anything. And if we're proving that \(p\) is false, we should be able to reach a contradiction if we assume that it's true! This is the essence of a proof by contradiction.

\[
\frac{
\Gamma, p \vdash \forall x (x)
}{
\Gamma \vdash \neg p
} ({\neg} R)
\]

Now, we have rules for every type of formula. One for using it as an assumption and one for proving it. We also have a special rule, \((I)\), for when the statement we want to prove is already assumed to be true.

I also mentioned some rules for re-ordering and duplicating assumptions. These are called structural rules. Here they are:

\[
\frac{
\Gamma,p,p \vdash q
}{
\Gamma,p \vdash q
} (CL)
\]

This allows us to duplicate assumptions so we don't have to "lose" them when we use them. All of our \(L\) rules replace the used assumption with something else. But the more assumptions we have, the easier it is to prove things. So we will implicitly invoke this rule when we invoke \(L\) rules to avoid losing assumptions.

\[
\frac{
\Gamma_1,q,p,\Gamma_2 \vdash r
}{
\Gamma_1,p,q,\Gamma_2 \vdash r
} (PL)
\]

This allows us to permute, or reorder our assumptions. In other words, the order of assumptions does not matter.

These two rules allow us to treat the context of assumptions as a set where we pretty much just keep adding assumptions as we go up the inference tree.

We can also forget assumptions if we want to for some reason:

\[
\frac{
\Gamma \vdash q
}{
\Gamma,p \vdash q
} (WL)
\]

Another rule we can add for convenience is the \((Cut)\) rule:

\[
\frac{
\Gamma \vdash q \qquad \Gamma,q \vdash p
}{
\Gamma \vdash p
} (Cut)
\]

This means, if we prove some auxiliary statement \(q\), often called a lemma, we can use it as an assumption to prove our original statement \(p\). This rule is technically not necessary, but is often convenient to have and can save us from duplicating proofs.

@;TODO free and bound vars
@;TODO equality
@;TODO talk about how we use and prove top and bottom
