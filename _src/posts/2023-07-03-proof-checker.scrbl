#lang scribble/manual

Title: Creating a Proof Checker
Date: 2023-07-03T22:57:22
Tags: DRAFT, math, projects, tutorials

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

In this post, we will build math from the ground up. We will lay the foundation for our proof checker by creating a language for mathematical statements and a set of rules for proving statements. In part 2, we will implement a proof checker based on this language and rules.

@; TODO verify prereqs at the end

@section{A Language for Facts}

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

@section{Rules for Reasoning}

We now have a language for stating mathematical facts. Now, we need a set of rules for proving that statements are true. These are our rules for reasoning.

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

Notice that we "lost" the conjunction assumption when we used \(({\wedge}L)\). In this case, that's fine. But for other \(L\) rules, we might want to use an assumption more than once. Later on, we'll introduce a rule for duplicating assumptions.

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

Our \(({\forall} L)\) rule is written as if the universal quantification has to be the last element of the context, but order of assumptions doesn't matter, so it can be anywhere in the context for the rule to be applicable. Later on, we'll justify this be adding a rule that allows us to re-order the asusmptions of the context.

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

Notice that there are no subproofs. In fact, all valid proofs will end with rules like this one, which have no subproofs. In this way, a proof is a tree of inferences, where each node is a judgement and an application of a rule, and its children are the sub-proofs required by the rule. The leaves of this tree are applications of rules like \((I)\) which require no subproofs. We'll come back to this fact when we implement our proof checker. For now, let's just realize that proofs are tree-like.

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

What about \(({\forall} R)\)? How do we prove that something is true about all things? In order to do that, we can instantiate the forall with some new variable that has no current meaning and no assumptions about it. Here's what this looks like:

\[
\frac{
\Gamma \vdash p[y \mathbin{/} x]
}{
\Gamma \vdash \forall x (p)
} ({\forall} R)
\]

We must add the restriction that \(y\) must not be mentioned anywhere in our context of assumptions \(\Gamma\) or our statement \(\forall x (p)\). More precisely, \(y\) must not occur free in \(\Gamma\) or \(\forall x (p)\). This restriction is necessary because if we are proving that \(p\) is true for all \(x\), we must be able to prove \(p[y \mathbin{/} x]\) without knowing or assuming anything about \(y\). This makes sense because if we prove that it is true for \(y\) without knowing anything about \(y\), then \(y\) could've been anything and we could've used the exact same proof.

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

If \(y\) was some other statement, even nonsense, we could've used the exact same proof structure:

\[
\displaystyle
\frac{
\displaystyle
\frac{
}{
\displaystyle
(x \in \neg x) \vdash (x \in \neg x)
} (I)
}{
\displaystyle
\vdash (x \in \neg x) \rightarrow (x \in \neg x)
} ({\rightarrow} R)
\displaystyle
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

All that's left is negation. Instead of thinking about \(\neg p\) directly, let's think about an equivalent form, \(p \rightarrow \forall x (x)\). To convince ourselves that these two forms are the same, let's think about the case when \(p\) is true. This implies \(\forall x (x)\), which is false. So when \(p\) is true, \(p \rightarrow \forall x (x)\) is false. This is just like with \(\neg p\). So far so good. When \(p\) is false, we essentially have \(\forall x (x) \rightarrow \forall x (x)\), which is true since every statement implies itself. So when \(p\) is false, \(p \rightarrow \forall x (x)\) is true. This is just like \(\neg p\). This is hand-wavy, but since we're making the rules here, we can just define \(\neg p\) to be \(p \rightarrow \forall x (x)\) and be happy that it fits our intuition for how negation should behave. In fact, if we wanted to, we could define everything, even \(\exists\), in terms of \(\forall\) and \(\rightarrow\).

Anyway, how can we use the implication \(p \rightarrow \forall x (x)\) as an assumption?

\[
\displaystyle
\frac{
\displaystyle
\frac{\cdots}{\Gamma \vdash p}(?)
\qquad
\displaystyle
\frac{
\displaystyle
\frac{}{\Gamma,q \vdash q}(I)
}{
\Gamma, \forall x (x) \vdash q
}({\forall} L)
}{
\displaystyle
\Gamma, p \rightarrow \forall x (x) \vdash q
} ({\rightarrow} L)
\]

If we use \(({\rightarrow} L)\) and prove that \(p\) is true, then we can assume that \(\forall x (x)\) is true. Then, we can use \(({\forall} L)\) to prove anything! In other words, if we reach a contradiction, all bets are off. This is a contradiction because we assumed that \(p\) is false and then proved that it is true. This gives us the rule

\[
\frac{
\Gamma \vdash p
}{
\Gamma, \neg p \vdash q
} ({\neg} L)
\]

One of the nice things about choosing \(\forall x (x)\) to represent "false" is that when we reach a contradiction (more precisely, we assume "false" is true), it is easy to use it to prove anything. And since we are defining negation in terms of our representation of "false", it wouldn't make sense to use \(x \wedge \neg x\) to represent "false" since it uses negation!

And how do we prove a negation \(\neg p\)? If that's the same as proving \(p \rightarrow \forall x (x)\), we have

\[
\displaystyle
\frac{
\displaystyle
\frac{\cdots}{\Gamma, p \vdash \forall x (x)} (?)
}{
\Gamma \vdash p \rightarrow \forall x (x)
} ({\rightarrow} R)
\]

Applying \(({\rightarrow} R)\), we get \(p\) as an assumption and we have to prove \(\forall x (x)\). This makes sense because the only way to prove \(\forall x (x)\) is if we reach a contradiction that allows us to prove anything. And if we're proving that \(p\) is false, we should be able to reach a contradiction if we assume that it's true! This is the essence of a proof by contradiction. Here is the rule:

\[
\frac{
\Gamma, p \vdash \forall x (x)
}{
\Gamma \vdash \neg p
} ({\neg} R)
\]

Now, we have rules for every type of formula. One for using it as an assumption and one for proving it (actually, two for proving \(\vee\)). We also have a special rule, \((I)\), for when the statement we want to prove is already assumed to be true.

I also mentioned some rules for duplicating and re-ordering assumptions. These are called structural rules. Here they are:

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

We can also forget assumptions if we don't need them:

\[
\frac{
\Gamma \vdash q
}{
\Gamma,p \vdash q
} (WL)
\]

This is valid because if we can prove that \(q\) is true without assuming \(p\), then that is also a valid proof if you assume \(p\).

Another rule we can add for convenience is the \((Cut)\) rule:

\[
\frac{
\Gamma \vdash q \qquad \Gamma,q \vdash p
}{
\Gamma \vdash p
} (Cut)
\]

This means if we prove some auxiliary statement \(q\), often called a lemma, we can use it as an assumption to prove our original statement \(p\). This rule is technically not necessary. It can be shown that any proof that uses it can be translated to a proof that doesn't use it. But is often convenient to have and can save us from duplicating proofs. This rule also allows us to build up our knowledge. For example, if we think of \(q\) as a theorem, this means if we prove a theorem, we can assume that the theorem is true to prove new theorems.

For those who want to learn more about this kind of system of rules, look up "Sequent Calculus".

@subsection{Tying up Loose Ends}

Now, let's tie up some loose ends. First, let's talk about free variables and substitution.

In the formula \(\forall x (p)\), we say that \(x\) is bound in \(p\). In other words, \(x\) is in scope for the formula \(p\). This means that when \(p\) references the variable \(x\), it is specifically referring to the universally quantified variable \(x\). For example, in the formula \(\forall x (x \rightarrow x)\), the \(x\) in the implication is the universally quantified \(x\). In contrast, if we consider the formula \(x \wedge y\) in isolation, \(x\) and \(y\) have no meaning. They are free variables. They are not bound. We say that \(x\) occurs free in \(x \wedge y\) and \(x\) occurs bound in \(\forall x (x \rightarrow x)\). In other words, a variable occurs free in an expression when it is referenced in a place where it is not bound by anything like a quantification.

Let's define this for each type of formula:

Let's say we're asking whether the variable \(x\) occurs free in some formula.

For a variable formula, \(x\) occurs free in the formula if the formula is \(x\). For example, \(x\) occurs free in the formula \(x\), but \(x\) does not occur free in the formula \(y\).

For a quantification, if the name of the quantified variable is \(x\), then \(x\) does not occur free. For example, \(x\) does not occur free in \(\forall x (x \rightarrow x)\) because its references are bound by the quantifier. In contrast, \(x\) does occur free in \(\forall y (x \rightarrow x)\) because the quantifier does not bind \(x\). So, in general, \(x\) occurs free in a quantifier if the quantified variable is not \(x\) and \(x\) occurs free in the body formula.

For a connective or an operator like \(p \rightarrow q\), \(x\) occurs free in that formula if \(x\) occurs free in any of the sub-formulas.

Here are some more examples:

The variables \(x\) and \(y\) both occur free in \(x \wedge y\).

The variable \(z\) does not occur free in \(x \wedge y\).

The variable \(x\) does not occur free in \(\forall x (x \wedge y)\), but the variable \(y\) does.

The variable \(x\) occurs free in \(x \wedge \forall x (x)\). Although it is bound in the forall, it is also referenced outside of the forall, where it is free.

A variable occurs free in a context if it occurs free in any formula in the context.

Hopefully, the restrictions on \(({\forall} R)\) and \(({\exists} L)\) make sense now. If \(y\) occurred free in the context, we'd know something about \(y\).

Now, let's talk about substitution. Recall that the notation for substitution is \(p[t \mathbin{/} x]\), meaning replace \(x\) with the formula \(t\) in the formula \(p\). For example, \((x \wedge y)[z \rightarrow z \mathbin{/} x]\) would be \((z \rightarrow z) \wedge y\). Let's define substitution:

For a variable formula, \(x[t \mathbin{/} x]\) becomes \(t\). But if the variable is different, \(y[t \mathbin{/} x]\) is just \(y\).

For a quantification, \((\forall y (p))[t \mathbin{/} x]\) becomes \(\forall y (p[t \mathbin{/} x])\). We just substitute in the body. But if the quantified variable is \(x\), we don't substitute. So \((\forall x (p))[t \mathbin{/} x]\) stays \(\forall x (p)\). This is because we only want to substitute free variables. Since \(x\) takes on a new meaning in the body of the quantifier, it shouldn't get substituted. Another problem is that \(x\) could occur free in \(t\). For example, in the subtitution \((\forall y (y \wedge x))[y \mathbin{/} x]\), we get \(\forall y (y \wedge y)\). We substituted a free variable \(y\) for \(x\), but it ended up bound after the subtitution. This is a problem because \(y\) could mean something outside of this quantification, but when it gets substituted in, its meaning gets changed to the universally quantified variable. This problem is known as name capture. For now, we will just say that this substitution is illegal.

For a connective or operator like \(p \rightarrow q\), we just substitute in each sub-formula.

Another thing we briefly mentioned is equivalence. What does it mean for two statements to be equivalent? There are a few ways to interpret this. We can define equivalence to mean that two formulas must be identical. But this is unnecessarily restrictive. Under this definition, \(x \wedge y\) would not be equivalent to \(y \wedge x\) even though those two statements feel like they should be equivalent. Another way of defining equivalence is that \(p\) and \(q\) are equivalent if \(p\) is true when \(q\) is true and vice versa. Symbolically, \((p \rightarrow q) \wedge (q \rightarrow p)\). This also means that if one statement is false, the other must be false. To see why, let's say \(p\) is false and assume \(q\) is true. But our double implication says that \(q\) implies \(p\), so if \(q\) is true, \(p\) must also be true. This is a contradiction, so either \(p\) must be true or \(q\) must be false. Our equivalence statement is true when both statements are true or when both statements are false. This is called a bidirectional implication, written \(p \leftrightarrow q\), and this is our notion of equivalent statements. We can prove that statements like \(x \wedge y\) and \(y \wedge x\) are equivalent by proving that bidirectional implication. People often read \(\leftrightarrow\) as "if and only if".

This notion of equivalence is useful, but it only makes sense for statements because implication and conjunction only make sense for statements. What if we want to say that two sets are the same? Or that two numbers are the same? We need a notion of equality to state that two objects are the same. How should we define equality?

First let's define how we can write down an equality. We will write an equality like \(t_1 = t_2\), where \(t_1\) and \(t_2\) are "terms". A term is just a formula, but we use the word "term" when the formula might represent some object like a set or a number, not necessarily a statement.

Ok, now that we have a way to state an equality, how should we define it? Let's start by listing some properties we want equality to have: Equality should be reflexive. So \(x = x\). Equality should be symmetric, so \(x = y \rightarrow y = x\). Equality should also be transitive, so \(x = y \wedge y = z \rightarrow x = z\). Another useful property to have is the subsitution property of equality. For example, if we have two equations,  \(x = y + 1\) and \(x + y = 2\), we should be able to substitute the first equation into the second and get \(y + 1 + y = 2\). Now that we have some general idea of how we want our equality operator to behave, let's write some rules:

A good first step is to make \(L\) and \(R\) rules, just like our logical connectives. Let's start with \(({=}R)\). How do we prove that two things are equal? In the simplest case, it should always be true that something is equal to itself. That's our reflexive property. As a rule, we write

\[
\frac{}{\Gamma \vdash t = t} ({=}R)
\]

Like \((I)\), this requires no subproofs. Before we think about other ways we might prove that two things are equal, let's think about how we can use an assumed equality. If we assume that \(x = y\), we can use the substitution property of equality to substitute \(x\) with \(y\) or vice versa. Anywhere we see an \(x\), we can replace it with a \(y\). In a proof, we'd run this substitution on the statement we're trying to prove. Here is the rule:

\[
\frac{\Gamma \vdash p[t \mathbin{/} x]}{\Gamma,x=t \vdash p} ({=}L)
\]

What about symmetry and transitivity? They actually follow from these two rules! Let's prove that this is true, starting with symmetry:

\[
\displaystyle
\frac{
\displaystyle
\frac{}{\vdash y=y} ({=}R)
}{
\displaystyle
x=y \vdash y=x
} ({=}L)
\]

We substituted \(y\) for \(x\) in the equation we're trying to prove. Now for transitivity:


\[
\displaystyle
\frac{
\displaystyle
\frac{
}{
y=z \vdash y=z
} (I)
}{
\displaystyle
x=y,y=z \vdash x=z
} ({=}L)
\]

Cool! But there is a slight problem. Since substitution is only defined for replacing a variable, if we want to use an equation with non-variable terms on both sides like \(x + y = y + x\), we wouldn't be able to substitute these two terms for each other. To fix this, let's extend our definition of substitution to be able to replace terms as well as variables.

We will add a case: \(t_1[t_2 \mathbin{/} t_1]\) becomes \(t_2\). If the whole formula is identical to the term we're replacing, we just replace the whole formula. For example, \((x \rightarrow y)[a \wedge b \mathbin{/} x \rightarrow y]\) becomes \(a \wedge b\).

The variable case and the simple connectives cases stay the same. For quantifiers, we had the rule \(\forall x (p)[t \mathbin{/} x]\) remains unchanged, but \(\forall y (p)[t \mathbin{/} x]\) becomes \(\forall y (p[t \mathbin{/} x])\). We needed to add that case where the variable being replace is the same as the quantified variable because we only wanted to substitute free occurrences of the variable, and the quantifier binds the variable, giving it a new meaning. To generalize this to term subsitution, \((\forall x (p))[t_2 \mathbin{/} t_1]\) will remain unchanged if \(x\) occurs free in \(t_1\), otherwise it will become \(\forall x (p[t_2 \mathbin{/} t_1])\). For example, \((\forall x (x \wedge (x \rightarrow y)))[z \mathbin{/} x \rightarrow y]\) remains \(\forall x (x \wedge (x \rightarrow y))\) because \(x\) occurs free in \(x \rightarrow y\). And again, to avoid name capture, the substitution is illegal if \(x\) occurs free in \(t_2\).

Now that we have extended our definition of substitution, we can write a more general \(({=}L)\) rule:

\[
\frac{\Gamma \vdash p[t_2 \mathbin{/} t_1]}{\Gamma,t_1=t_2 \vdash p} ({=}L)
\]

We just extended our language to include equality and all we needed to do was add a rule for how to prove an equality and one for how to use an equality. This is a common pattern.

We now have a language for stating mathematical facts, a set of rules for proving statements in that language, and an idea of how to extend it as needed to incorporate new kinds of statements. Now, let's explore how to apply this system to do some math!

@section{Theories}

@subsection{Set Theory}

Let's talk about sets. What is a set? Intuitively, we think of a set as an unordered collection of things. Something is either in a set or not in a set. We will write set membership like \(x \in S\), which means \(x\) is an element of the set \(S\). This is a new type of formula in our language. Last time we added something new, equality, we added rules for how to prove it and how to use it as an assumption. For set membership, we won't be doing that. You can't really use the fact that an object is in a set on its own like you could with an equality. There is also no inherent way to prove set membership like there is for equality. Instead, we will write some axioms that describe the nature of set membership. This is how we will define what a set is. We will be using some of the axioms of Zermelo-Fraenkel set theory.

First up, we have the axiom of extensionality:

\[
\forall x (\forall y ([\forall z (z \in x \leftrightarrow z \in y)] \rightarrow x = y))
\]

That's pretty dense. Let's translate to English: For any two objects \(x\) and \(y\), if, for all objects \(z\), \(z \in x\) if and only if \(z \in y\), then \(x\) and \(y\) are the same set. In other words, two sets are equal when they have the same elements.

Next, we have the axiom of empty set:

\[
\exists \emptyset (\forall x (\neg (x \in \emptyset)))
\]

This states that there exists a set with no elements.

Next, we have the axiom of pairing:

\[
\forall x (\forall y (\exists z (\forall e (e \in z \leftrightarrow e = x \vee e = y))))
\]

This is saying that, for any two things \(x\) and \(y\), there exists a set \(z\) containing exactly those two objects.

Next, we have the axiom of union:

\[
\forall F (\exists A (\forall Y (\forall x ((x \in Y \wedge Y \in F) \leftrightarrow x \in A))))
\]

Here, \(F\) is some set of sets, \(Y\) is an element of \(F\), \(x\) is an element of \(Y\), and \(A\) is the union of sets that are elements of \(F\). All this is saying is that for all sets of sets \(F\), there exists some set \(A\) that is the union of all sets that are elements of \(F\). For example, if we had \(F = \{\{a,b,c\}, \{b,c,d\}, \{a\}\}\), we'd have \(A = \{a,b,c,d\}\). The property that defines \(A\) is that elements of \(A\) are elements of elements of \(F\).

Next, we have the axiom of power set:

\[
\forall x (\exists y (\forall z ([\forall e (e \in z \rightarrow e \in x)] \leftrightarrow z \in y)))
\]

The power set of a set \(x\) is the set of all subsets of \(x\). \(z\) is a subset of \(x\) if and only if every element of \(x\) is an element of \(z\). That's what \(\forall e (e \in z \rightarrow e \in x)\) is saying. For example, the power set of \(\{1,2,3\}\) is \(\{\{\}, \{1\}, \{2\}, \{3\}, \{1,2\}, \{1,3\}, \{2,3\}, \{1,2,3\}\}\). Notice that the empty set \(\{\}\) and the set itself \(\{1,2,3\}\) are subsets of \(\{1,2,3\}\). This axiom is saying that for all sets \(x\), the power set of \(x\), \(y\), exists.

Next, we have the axiom schema of specification:

Let \(p\) be some formula with free variables \(x,z,w_1,w_2,\ldots,w_n\) where \(y\) does not occur free in \(p\).

\[
\forall z (\forall w_1 (\forall w_2 (\ldots \forall w_n (\exists y (\forall x (x \in y \leftrightarrow (x \in z \wedge p)))))))
\]

This is hard to read, but the idea is actually quite simple. If we have some set \(z\), there exists a set \(y\) which is a subset of \(z\) that contains elements of \(z\) that have some property \(p\). We often see this written like \(y = \{x \in z : p\}\). In other words, we can construct a set \(y\) from a set \(z\) by "filtering" its elements. You might be wondering why we can't just construct a set \(\{x : p\}\) of all objects with some property. If we allowed that, we could write \(\{x : \neg (x \in x)\}\). This is the set of all sets that do not contain themselves. This is Russel's paradox and it leads to a contradiction. To avoid this, we restrict ourselves to constructing subsets of existing sets.

This is called an axiom schema because there is an axiom for every formula \(p\) where \(y\) does not occur free.

There are more axioms in Zermelo-Fraenkel set theory, but we'll stick to these for now.

These axioms describe what equality means for sets and states what sets can exist. We can express ideas like subsets, power sets, intersections, and unions using logic and our membership operator.

How can we use these axioms to prove things about sets? For the axioms, we can just put them in our context of assumptions. For an axiom schema like the axiom schema of specification, we can use a rule:

\[
\frac{
\Gamma,\exists y (\forall x (x \in y \leftrightarrow (x \in z \wedge p))) \vdash q
}{
\Gamma \vdash q
} (Spec)
\]

With the restriction that \(y\) does not occur free in \(p\)

This allows us to add the axiom (with the foralls of \(z\) and the free variables already instantiated) for some formula \(p\) to our context at any time.

As an example, let's prove the existence of binary set intersections. That is, given two sets, there exists a set whose elements are in both sets. We'll use the axiom schema of specification.

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
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{}{
\displaystyle
d \in c \leftrightarrow (w \in a \wedge w \in b) \vdash d \in c \leftrightarrow (w \in a \wedge w \in b)
} (I)
}{
\displaystyle
\forall x (x \in c \leftrightarrow (x \in a \wedge x \in b)) \vdash d \in c \leftrightarrow (w \in a \wedge w \in b)
} ({\forall}L)
}{
\displaystyle
\forall x (x \in c \leftrightarrow (x \in a \wedge x \in b)) \vdash  \forall w (w \in c \leftrightarrow (w \in a \wedge w \in b))
} ({\forall}R)
}{
\displaystyle
\forall x (x \in c \leftrightarrow (x \in a \wedge x \in b)) \vdash \exists z (\forall w (w \in z \leftrightarrow (w \in a \wedge w \in b)))
} ({\exists}R)
}{
\displaystyle
\exists y (\forall x (x \in y \leftrightarrow (x \in a \wedge x \in b))) \vdash \exists z (\forall w (w \in z \leftrightarrow (w \in a \wedge w \in b)))
} ({\exists}L)
}{
\displaystyle
\vdash \exists z (\forall w (w \in z \leftrightarrow (w \in a \wedge w \in b)))
} (Spec)
}{
\displaystyle
\vdash \forall y (\exists z (\forall w (w \in z \leftrightarrow (w \in a \wedge w \in y))))
} ({\forall}R)
}{
\displaystyle
\vdash \forall x (\forall y (\exists z (\forall w (w \in z \leftrightarrow (w \in x \wedge w \in y)))))
} ({\forall}R)
\]

We used specification on the set \(a\) and our property \(p\) was \(x \in b\).

As another example, let's prove the existence of singleton sets. That is, given a thing, there exists a set containing only that thing. We'll use the axiom of pairing.

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
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
\displaystyle
\frac{
}{
\displaystyle
v \in w \vdash v = w
} (I)
\qquad
\displaystyle
\frac{
\displaystyle
\frac{}{
\displaystyle
v = a, v \in w \vdash v = a
} (I)
\qquad
\displaystyle
\frac{}{
\displaystyle
v = a, v \in w \vdash v = a
} (I)
}{
\displaystyle
v = a \vee v = a, v \in w \vdash v = a
} ({\vee}L)
}{
\displaystyle
v \in w \leftrightarrow v = a \vee v = a, v \in w \vdash v = a
} ({\rightarrow}L)
}{
\displaystyle
v \in w \leftrightarrow v = a \vee v = a \vdash v \in w \rightarrow v = a
} ({\rightarrow}R)
}{
\displaystyle
\forall e (e \in w \leftrightarrow e = a \vee e = a) \vdash v \in w \rightarrow v = a
} ({\forall}L)
}{
\displaystyle
\forall e (e \in w \leftrightarrow e = a \vee e = a) \vdash \forall z (z \in w \rightarrow z = a)
} ({\forall}R)
}{
\displaystyle
\forall e (e \in w \leftrightarrow e = a \vee e = a) \vdash \exists y (\forall z (z \in y \rightarrow z = a))
} ({\exists}R)
}{
\displaystyle
\exists z (\forall e (e \in z \leftrightarrow e = a \vee e = a)) \vdash \exists y (\forall z (z \in y \rightarrow z = a))
} ({\exists}L)
}{
\displaystyle
\forall y (\exists z (\forall e (e \in z \leftrightarrow e = a \vee e = y))) \vdash \exists y (\forall z (z \in y \rightarrow z = a))
} ({\forall} L)
}{
\displaystyle
\forall x (\forall y (\exists z (\forall e (e \in z \leftrightarrow e = x \vee e = y)))) \vdash \exists y (\forall z (z \in y \rightarrow z = a))
} ({\forall} L)
}{
\displaystyle
\forall x (\forall y (\exists z (\forall e (e \in z \leftrightarrow e = x \vee e = y)))) \vdash \forall x (\exists y (\forall z (z \in y \rightarrow z = x)))
} ({\forall} R)
\]

That's a lot, but the essense of the proof is that we use the axiom of pairing to create a set containing \(x\) and \(x\), which is the same as the set containing just \(x\). It took a lot to prove such a simple theorem, but we can be sure that our proof is valid since it followed our rules.

You might be wondering where symbols like \(\subseteq\) and curly brace notation \(\{\}\) is. We actually don't need it. We can express these ideas in terms of logic and the membership \(\in\) operator. But it is convenient to be able to write things down with custom notation, and as long as we're careful, this won't break anything.

We can introduce a notation by introducing an axiom that defines its meaning. For example, for subset, we can introduce the axiom

\[
\forall x (\forall y (x \subseteq y \leftrightarrow \forall z (z \in x \rightarrow z \in y)))
\]

This is just defining a new operator in terms of existing operators.

For something like curly brace notation \(\{x,y,z\}\), we can think of it as a variadic operator. Its defining property is

\[
\forall x_1 (\forall x_2 (\ldots \forall x_n (\forall y (y \in \{x_1,x_2,\ldots,x_n\} \leftrightarrow y = x_1 \vee y = x_2 \vee \ldots \vee y = x_n))))
\]

We can't write a single axiom for this since we want an axiom for each \(n\). So we can add a rule instead like for the axiom schema of specification.

We have to be careful when we introduce notation like this which constructs objects. Notation should just be shorthand. It shouldn't allow us to prove anything that we couldn't prove before. So if you introduce notation that is shorthand for an object with some property, you should first prove that an object with that property exists. For curly brace notation, we should prove

\[
\forall x_1 (\forall x_2 (\ldots \forall x_n (\forall y (\exists z (y \in z \leftrightarrow y = x_1 \vee y = x_2 \vee \ldots \vee y = x_n)))))
\]

We can do this using the axiom of pairing and the axiom of union.

What about that notation for specification, \(\{x \in z : p\}\)? We have to be careful. In the previous operators, in order to compute free variables and substitution, we can just recur on the sub-formulas. But here, if we tried to substitute something for \(x\) in this term, we wouldn't want the \(x\) to be replaced. \(x\) is bound in \(p\). This notation is like a quantification over x. Are we going to have to specify free variables and substitution on every operator we define? Let's keep things simple for now and restrict ourselves to operators that don't bind any variables.

A theory like set theory can be expressed with a collection of axioms, operators, and rules.

@subsection{Natural Numbers}

We talked about sets, now let's finally talk about numbers! Specifically, natural numbers. That is, \(0,1,2,3,\ldots\).

We'll start out by stating that the constant zero is a natural number. To do that, we'll introduce a new operator, \(N\). This is a unary operator, which means it has one argument. \(N(x)\) means that \(x\) is a natural number.

Our first axiom states that 0 is a natural number:

\[N(zero)\]

We're using a free variable, \(zero\) to represent the number 0.

So far so good. We will also introduce a successor operation, \(S\), which is another unary operator. \(S(x)\) is the successor of \(x\). We define the number 1 to be the successor of 0, \(S(zero)\). And we define 2 to be the successor of 1, \(S(S(zero))\). And so on. The successor operation behaves like adding 1.

Our second axiom states that the natural numbers are closed under succession:

\[\forall n (N(n) \rightarrow N(S(n)))\]

If \(n\) is a natural number, then its successor is a natural number.

Our third axiom tells us about equality of natural numbers:

\[\forall n (\forall m (N(n) \wedge N(m) \rightarrow (S(n) = S(m) \rightarrow n = m)))\]

If the successors of two natural numbers are equal, then the two numbers are equal.

We also state that 0 is not the successor of any natural number:

\[\forall n (N(n) \rightarrow \neg (S(n) = zero))\]

Finally, we have the axiom schema of induction:

Let \(p\) be some formula where \(n\) does not occur free.

\[[p[zero \mathbin{/} x] \wedge \forall n (p[n \mathbin{/} x] \rightarrow p[S(n) \mathbin{/} x])] \rightarrow \forall x (N(x) \rightarrow p)\]

If \(p\) is true for 0 and, for all natural numbers \(n\), \(p\) being true for \(n\) implies that \(p\) is true for \(S(n)\), then \(p\) is true for all natural numbers. This is how we prove that things are true for all natural numbers. Here is the rule:

\[
\frac{
\Gamma \vdash p[zero \mathbin{/} x]
\qquad
\Gamma, N(n), p[n \mathbin{/} x] \vdash p[S(n) \mathbin{/} x]
}{
\Gamma \vdash \forall x (N(x) \rightarrow p)
} (NatInd)
\]

With the restriction that \(n\) does not occur free in \(p\). This form follows from the axiom schema if we apply some rules.

These are the peano axioms for natural numbers. Normally, people also include axioms that assert reflexive, symmetric, and transitive equality, and that natural numbers are closed under equality \(\forall n (\forall a ((N(n) \wedge n = a) \rightarrow N(a)))\), but these all follow from our definition of equality, so we don't need these axioms.

We can define addition and multiplication by adding two operators \(+,\cdot\) and some axioms:

\[\forall a (N(a) \rightarrow a + 0 = a)\]
\[\forall a (\forall b(N(a) \wedge N(b) \rightarrow a + S(b) = S(a + b)))\]
\[\forall a (N(a) \rightarrow a \cdot  0 = 0)\]
\[\forall a (\forall b(N(a) \wedge N(b) \rightarrow a \cdot S(b) = a + a \cdot b))\]

@;TODO prove that 1 + 1 = 2

For some intuition, let's look at some examples:

\[a + S(S(S(zero)))\]
\[S(a + S(S(zero)))\]
\[S(S(a + S(zero)))\]
\[S(S(S(a + zero)))\]
\[S(S(S(a)))\]

\(a + b\) just applies \(S\) to \(a\) \(b\) times. So addition is repeated succession.

\[a \cdot S(S(S(zero)))\]
\[a + a \cdot S(S(zero))\]
\[a + a + a \cdot S(zero)\]
\[a + a + a + a \cdot zero\]
\[a + a + a + zero\]
\[a + a + a\]

\(a \cdot b\) just adds \(a\) to itself \(b\) times. So multiplication is repeated addition.

If you want some practice, prove that the natural numbers are closed under addition: \(\forall a(N(a) \rightarrow \forall b (N(b) \rightarrow N(a + b)))\). Hint: you only need to do induction on \(b\).

Now we see how we can use our system to talk about pretty much any area of math. In this post, we built a foundation for mathematics where we can make statements and prove them. In the next part, we will implement this system in code so we can have a computer automatically check our proofs.

@section{Extras}

Here is some extra exploration of the concepts in this post:

@subsection{Natural Numbers as Sets}

We can actually define natural numbers in terms of sets. And all we need to do is add a few axioms:

\[
zero = \{\}
\]

\[
\forall n (S(n) = \bigcup \{n, \{n\}\})
\]

Where \(\bigcup\) denotes the union of a set's elements, like in the axiom of union. To define this operator, we'll add the axiom

\[
\forall F (\forall x (x \in \bigcup F \leftrightarrow \exists Y (x \in Y \wedge Y \in F)))
\]

This means that

\[
0 = \{\}
\]

\[
1 = \{\{\}\} = \{0\}
\]

\[
2 = \{\{\}, \{\{\}\}\} = \{0,1\}
\]

\[
3 = \{\{\}, \{\{\}\}, \{\{\}, \{\{\}\}\}\} = \{0,1,2\}
\]

Each natural number is the set of all lesser natural numbers.

Rather than \(S\) being a primitive operator, it is now defined in terms of union and our curly brace notation, which is defined in terms of logic and set membership.

We can also define \(N\) in terms of sets:

\[\forall n (N(n) \leftrightarrow n = \{\} \vee \exists m (N(m) \wedge n = S(m)))\]

This definition is equivalent to the original definition under our new axioms.

Now that we have notation, we can more easily express the axiom of infinity, one of the axioms we left out of our set theory:

\[
\exists X (\{\} \in X \wedge \forall y (y \in X \leftrightarrow S(y) \in X))
\]

This axiom guarantees the existence of an infinite set. You might be surprised to see \(S\) show up in there, but now it's defined in terms of sets, so it is valid to use in set theory. But this looks like it's saying something about natural numbers. We can also see that zero, which we have now defined as \(\{\}\), shows up too. In fact, this set is exactly the set of natural numbers! We can prove that all natural numbers are in this set by induction. To give an idea of the proof, 0 is in the set by the first condition \(\{\} \in X\) and the second condition is basically an inductive case. Proving that all elements of this set are natural numbers is less straightforward.

@; TODO prove the other direction to yourself at least.

We can then define \(N\) using the axiom of infinity. First, we add the axiom defining \(\mathbb{N}\) to be the set whose existence is guaranteed by this axiom:

\[
\{\} \in \mathbb{N} \wedge \forall y (y \in \mathbb{N} \leftrightarrow S(y) \in \mathbb{N})
\]

This is the set of natural numbers.

Now we can define \(N\) in terms of \(\mathbb{N}\):

\[
\forall n (N(n) \leftrightarrow n \in \mathbb{N})
\]

Another thing we get for free is comparison \(<\). Our definition of natural numbers leads to the fact that every natural number is the set of all natural numbers less than itself. This means we can define natural number comparison in terms of set membership:

\[
\forall a (\forall b (N(a) \wedge N(b) \rightarrow (a < b \leftrightarrow a \in b)))
\]

Another consequence of using set theory is that we no longer need the axioms that state that zero is a natural number, the successor of a natural is a natural, or even the axiom that states that naturals are closed under equality. These statements which we previously needed to assume as axioms can now be proven using set theory and our new definitions of \(S\) and \(N\).

@;TODO do you need regularity to prove that zero is not the successor of any natural?
@;TODO do you even need the axiom of induction?

Set theory is pretty cool! In fact, pretty much all modern mathematics can be expressed in terms of set theory. Real numbers can be defined in terms of rationals, which can be defined in terms of integers, which can be defined in terms of naturals, which can be defined in terms of sets. In geometry, shapes and lines can be represented by sets of points in space. Points in space can be defined in terms of ordered pairs, which can be defined in terms of sets. This is why people say that logic and set theory are the foundation of mathematics.

@;TODO where is N for sets?

@;TODO we can frame this all in terms of set theory by adding zero = {}, S(n) = n U {n}, N(n) <-> n = {} or exists m : N(n) and n = S(m) (or do axiom of infinity and say it's in the set of naturals). Then we get < is set membership and can ditch the successor equality axiom.


@;TODO add the AndL and WL to convert <-> to -> in singleton proof
@;TODO make the RHS <-> instead of -> in the singleton proof
@;TODO forall x x is nice because it uses no logical connectives? Yes. Since we define neg in terms of impl and false, it wouldn't make sense to use negation in the definition of false. Also mention that all you really need is forall and impl. Briefly mention this when discussing our choice for false too.
@;TODO talk about how we use and prove top and bottom
@;TODO explore defining everything in terms of impl and forall
