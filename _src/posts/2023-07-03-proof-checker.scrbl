#lang scribble/manual

Title: Checking Proofs
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

@section{First-order Logic}

In order to state facts, we need a language to write those facts in. For example, let's consider a statement written in English:

"All men are mortals. Socrates is a man. Therefore Socrates is mortal."

What exactly is the relationship between these sentences? What do "is" and "are" mean? You can look up the definitions for "is" and "are" in a dictionary, but those definitions are in terms of other imprecise english words. What we want are some well-defined connectives for building statements and relating objects. To this end, we will use the language of first-order logic.

Here is our example written in this language:

\[
(\forall person (person \in men \rightarrow person \in mortals) \wedge socrates \in men) \rightarrow socrates \in mortals
\]

This statements also uses the language of set theory, which we'll get into later. Let's go over all the symbols and their meaning:

\(\forall\) means "for all", \(\in\) means "is an element of", \(\rightarrow\) means "implies", and \(\wedge\) means "and". \(\forall\) is what is called a quantifier. Specifically, it is for universal quantification, which allows us to state that some property is true for all things. The rest of the symbols are operators and logical connectives. \(\in\) is from set theory and \(x \in S\) means \(x\) is an element of the set \(S\). \(p \rightarrow q\) can be read as "if p, then q". This statement is true when \(p\) being true implies that \(q\) is true. \(p \wedge q\) is read "p and q" and is true when both \(p\) and \(q\) are true. Putting it all together, we have "If, for all people, a person being a member of the set of men implies that the person is a member of the set of mortals and socrates is in the set of men, then socrates is in the set of mortals." We encode "is" with set membership and we have quantifiers and logical connectives for chaining facts together in precise and meaningful ways.

To determine whether this statement is true, we'll need to define how these quantifiers and connectives work. But first, let's fully describe our language of statements, ignoring set theory for now and just focusing on logic:

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

This means "For all x, for all S, x is an element of S or x is not an element of S". Is this true? We'll need set theory to figure that out.

Here is another example:

\[
\forall x (y)
\]

This means "For all x, y is true". This is nonsense. What is \(y\)? Whatever rules we create for using reasoning, we must ensure that nonsensical statements like this one cannot be proven to be true.

Let's go through each type of formula and think about what these statements mean:

Universal quantification is used to make a statement about all things. \(\forall x (p)\) means "for all x, the statement p is true", where \(p\) is some statement about \(x\). Here, I'm using \(x\) to represent a variable in our language, and I'm using \(p\) as a meta-variable to represent some formula/statement in our language. This is a subtle distinction. When you see something like \(x,y,z,S\), that means a variable in our language, and when you see \(p,q,r\), that means some formula, not literally the variable \(p\). So \(x\) is literally the formula \(x\), and \(p\) is a meta-variable representing some formula in our language.

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

@subsection{Rules for Reasoning}

For this section, we're going to temporarily forget about operations like set membership and focus on pure logical statements. This means the statements we'll be working with will be statements about statements, not statements about objects like sets. These statements about statements are totally valid, meaningful, and important, but they are abstract and can be hard to reason about.


@;TODO equality
