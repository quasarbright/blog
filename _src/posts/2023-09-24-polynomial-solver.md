    Title: Solving Polynomials with Recursion
    Date: 2023-09-24T12:43:02
    Tags: math, JavaScript

In this post, we explore a simple and elegant algorithm to find the (real) zeros of a polynomial using recursion and derivatives.

<!-- more -->



This post implements the algorithm described in [Carvalho, Osvaldo. (2017). A simple recursive algorithm to find all real zeros of a polynomial](https://www.researchgate.net/publication/320864673_A_simple_recursive_algorithm_to_find_all_real_roots_of_a_polynomial).

Finding the zeros of a polynomial is an important problem in math. For some polynomials, we can use techniques like factoring to find zeros, and for polynomials up to degree 4, we have formulas that find the zeros directly. But for higher degree polynomials, these techniques are impractical or impossible. There are many numerical methods for finding the zeros of polynomials. The one we explore in this post uses derivatives and recursion in an elegant way, and is relatively simple.

For this post, you'll to know a bit of math and coding. For the math, you'll pretty much just need to be familiar with polynomials and some basic calculus. For the coding, a basic understanding of JavaScript and some familiarity with algorithms should suffice.

# The Math

If we know that there is exactly one zero between two \\(x\\)-values, then we can use an algorithm like binary search to find it. However, if there are potentially multiple zeros in between, then we can only find one of them with a binary search, so we'll miss some zeros. The tricky part of solving polynomials is finding these intervals with exactly one zero in them, and that's where this algorithm comes in.

The key observation behind this algorithm is that the only points where a polynomial can change direction is when its derivative is zero. If a polynomial is increasing, its derivative is positive. The only way for it to start decreasing is for its derivative to become negative, and it must become zero at some point in between. Conversely, this means that between the zeros of a polynomial's derivative, the polynomial does not change direction. So if one of the zeros of the derivative is positive in the polynomial and an adjacent one is negative, there must be exactly one zero in betwen. And if they both have the same sign, we can be sure that there is not a zero in between. The only other place we can find a zero of the polynomial is beyond the leftmost or rightmost zeros of the derivative.


Let's look at an example:

<iframe src="https://www.desmos.com/calculator/0wvhj6rha8?embed" width="500" height="500" style="border: 1px solid #ccc" frameborder=0></iframe>
<!-- https://www.desmos.com/calculator/0wvhj6rha8 -->

In this graph, we see that the polynomial has 3 zeros. Zeros of the derivative are marked with blue dots. Visually, we can see that a zero of the derivative looks like the graph getting horizontal for a moment. The first zero of the graph is in the same place as one of the zeros of the derivative. The second zero is between the second and third zeros of the derivative. And the third zero of the graph is between the third zero of the derivative and positive infinity.

There is no zero between the first and second zero of the derivative. Zeros only occur between derivative zeros when the polynomial is positive at one zero and negative at the other.

Zeros can only occur on zeros of the derivative, between derivative zeros of opposite sign, between negative infinity and the minimum zero of the derivative, and between the maximum zero of the derivative and positive infinity. And since the graph can only change direction at a zero of the derivative, between two zeros, there can only be at most a single zero. So if we have the zeros of the derivative, we can easily find well-behaved intervals to search for the zeros. Within those intervals, we can just binary search to find the zero.

That's nice, but how do we find the zeros of the derivative? Well, the derivative of a polynomial is another polynomial. And we know how to find the zeros of a polynomial! Since the derivative of a polynomial is a smaller polynomial, this recursive algorithm will terminate. And the base case will be first degree polynomials, which we can easily solve directly with a formula:

$$ ax + b = 0 $$

$$ x = -\frac{b}{a} $$

For degree zero polynomials, we'll just treat them as if they have no zeros. After all, the two possibilities are having no zeros and having infinite zeros, and we can't return a list containing all real numbers!

# The Code

To start off, let's figure out how we're going to represent a polynomial. Remember, a polynomial looks like this:

$$ p(x) = 4x^2 + 3x - 2 $$

A polynomial is a sum of scaled integer exponents of its input \\(x\\). We can specify a polynomial by its coefficients. In this example, the degree 0 coefficient is \\(-2\\), the degree 1 coefficient is 3, and the degree 2 coefficient is 4. All other coefficients are 0. Remember, the degree of a term is the exponent that \\(x\\) is raised to, and the degree of an entire polynomial is the degree of its highest degree term with a non-zero coefficient.

To represent a polynomial, we will just use a list of its coefficients:

```ts
// The coefficient of degree i of polynomial p is p[i]
type Polynomial = number[]
```

We store its coefficients in order from lowest degree to highest. For example, our \\(p(x)\\) would look like this:

```ts
const p = [-2, 3, 4]
```

This is nice because the degree 0 coefficient is just `p[0]`.

Our solver will return a list of numbers representing the \\(x\\)-values where \\(p(x) = 0\\).

Here is the high-level algorithm:

```ts
// find all zeros of the polynomial, sorted.
// for p = 0, return no zeros
export function zeros(p: Polynomial): number[] {
    if(degree(p) == 0) {
        return []
    } else if (degree(p) === 1) {
        const [b, a] = p
        // 0 = ax + b
        // x = -b/a
        return [-b/a]
    } else {
        const dpdx = derivative(p)
        const derivativeZeros = zeros(dpdx)
        return [
            ...betweenZeros(p, derivativeZeros),
            ...endZeros(p,derivativeZeros),
        ].sort()
    }
}
```

We see our two bases cases, a degree 0 and a degree 1 polynomial, which we solve directly.

In the recursive case, we compute the derivative, `dpdx`, which is just another polynomial, find its zeros, and then use them to find the zeros of `p`.

That `...` stuff in the returned list just concatenates those two lists into one. Those helper functions `betweenZeros` and `endZeros` return lists of numbers, so we concatenate them. We also sort the list.

We'll build out our implementation top down like this, calling helper functions and then implementing them later.

First, let's write `degree`, a helper that computes the degree of a polynomial:

```ts
// the degree of the highest-degree term with non-zero coefficient
export function degree(p: Polynomial): number {
    let degree = 0
    for(let n = 0; n < p.length; n++) {
        if(p[n] !== 0) {
            degree = n
        }
    }
    return degree
}
```

We iterate through the coefficients in order from lowest degree to highest, updating the highest degree when we encounter a term with a non-zero coefficient.

Now, let's implement `derivative`, which computes the derivative of a polynomial.

Remember, the derivative of a polynomial term, also called a monomial, looks like this:

$$ \frac{d}{dx} ax^n = anx^{n-1} $$

And since a polynomial is the sum of monomials, we can just do this to each term and sum the results.

Since we represent a polynomial by a list of its coefficients of ascending degree, we can just multiply each coefficient by its degree and shift the list to the left!

For example:

$$ p(x) = 4x^2 + 3x - 2 $$
$$ p'(x) = 8x + 3 $$

```ts
const p = [-2, 3, 4]
const dpdx = [3, 8]
```

We multiplied 3 by 1 and 4 by 2, and shifted to the left, decreasing the degree of each term by one. The constant term `-2` vanished because its derivative is zero.

Here it is in code:

```ts
// the derivative of the polynomial, as another polynomial
export function derivative(p: Polynomial): Polynomial {
    return p.map((coefficient, degree) => coefficient * degree).slice(1)
}
```

Since the degree of a term is its index in the list, we use an indexed `map`. For those unfamiliar, the array method `map` in JavaScript returns a new list resulting from applying a function to every element in the list. For example:

```ts
[1,2,3,4].map(x => 2 * x) // [2,4,6,8]
[10,24,54,77].map((x,index) => index) // [0,1,2,3]
```

When the function we supply to `map` takes in two arguments, the second argument is the index of the element in the list.

The `slice` method can be used to "chop off" the beginning of a list.

```ts
[46,55,93,28].slice(1) // [55,93,28]
```

When you pass just one argument to slice, it will give you a list starting at that index of the original list. Since we want to shift the list to the left, we just chop off the first element. This has the effect of decreasing the degree of each term by one, which is exactly what the derivative does.

Now, let's implement one of the interesting bits, `betweenZeros`. This function finds the zeros on and between the zeros of the derivative:

```ts
// Find zeros between derivative zeros of opposite sign and on derivative zeros
function betweenZeros(p: Polynomial, derivativeZeros: number[]): number[] {
    const zeros = []
    for(let i = 0; i < derivativeZeros.length - 1; i++) {
        const left = derivativeZeros[i]
        const right = derivativeZeros[i+1]
        const isSignChange = evalPolynomial(p, left) * evalPolynomial(p, right) < 0
        if (isZero(p, left)) {
            zeros.push(left)
        }
        if (isZero(p, right)) {
            zeros.push(right)
            // skip the next pair of zeros to avoid duplicates
            i++
        } 
        if (!isZero(p, left) && !isZero(p, right) && isSignChange) {
            // sign change, there is a zero in between
            zeros.push(findZeroBetween(p, left, right))
        }
    }
    return zeros
}
```

Let's go step by step.

```ts
    for(let i = 0; i < derivativeZeros.length - 1; i++) {
        const left = derivativeZeros[i]
        const right = derivativeZeros[i+1]
```

First, we want to iterate over adjacent pairs of zeros so we can look in between them. This takes advantage of the assumption that the list of zeros is sorted. `left` is the \\(x\\)-value of the zero of the derivative on the left of the pair, and `right` is the one on the right. Between these two \\(x\\) values, there might be a zero.

To visualize this, let's bring back our graph:

<iframe src="https://www.desmos.com/calculator/0wvhj6rha8?embed" width="500" height="500" style="border: 1px solid #ccc" frameborder=0></iframe>

Remember, the blue dots are the zeros of the derivative. We can see that there is no zero of the polynomial between the first and second blue dot, but the blue dot itself is a zero. And between the second and third dot, there is a zero. In general, if and only if one of the elements of our pair is positive and the other is negative, there is a zero in between them.

```ts
        const isSignChange = evalPolynomial(p, left) * evalPolynomial(p, right) < 0
```

`evalPolynomial` is a helper that we'll write later. It evaluates the polynomial at some \\(x\\)-value.

Whether there is a zero between `left` and `right` can be determined by whether there is a sign change. For example, if the polynomial is negative on the left and positive on the right like between the second and third blue dots, there must be a zero in between! Conversely, if there isn't a sign change and both values are positive, both are negative, or one is zero like the first blue dot, then there is definitely not a zero between them. We handle the zero cases and the sign change cases, and ignore the case of both positive or both negative.

```ts
        if (isZero(p, left)) {
            zeros.push(left)
        }
```

If `left` is a zero, then we add it to the list This will happen on the first iteration in our graphed example since the first derivative zero is a zero of the polynomial. However,

```ts
        if (isZero(p, right)) {
            zeros.push(right)
            // skip the next pair of zeros to avoid duplicates
            i++
        } 
```

If `right` is a zero, we also add it to the list, but we also increment `i` to skip the next iteration of the loop. After all, `right` will become `left` next iteration, and we don't want to add it as a zero again. And the in-between case won't happen because `right` is a zero, so it's safe to skip. Now for the in between case:

```ts
        if (!isZero(p, left) && !isZero(p, right) && isSignChange) {
            // sign change, there is a zero in between
            zeros.push(findZeroBetween(p, left, right))
        }
```

If neither `left` nor `right` was a zero and there is a sign change like the second and third blue dots, we search in between `left` and `right` for a zero.

Since that's the most interesting helper, let's implement it next:

```ts
// find a zero of p between the two x values
// left doesn't actually have to be less than right
function findZeroBetween(p: Polynomial, left: number, right: number): number {
    // binary search
    if (evalPolynomial(p, left) * evalPolynomial(p, right) >= 0) {
        throw new Error("no sign change")
    }
    const middle = (left + right) / 2
    if (isZero(p, middle)) {
        return middle
    } else if (evalPolynomial(p, left) * evalPolynomial(p, middle) < 0) {
        // sign change between left and middle
        return findZeroBetween(p, left, middle)
    } else {
        // must be sign change between middle and right
        return findZeroBetween(p, middle, right)
    }
}
```

We binary search between `left` and `right` for a zero. We assert that there is a sign change between `left` and `right` because otherwise, there is no zero between them and we will search forever. We use sign changes to detect which half of the interval to recur on. The interval that has a sign change is the one that contains the zero.

Now, to tie up loose ends, let's implement `isZero` and `evalPolynomial`:

```ts
const TOLERANCE = 0.001
// Is it pretty much a zero?
function isZero(p: Polynomial, x: number): boolean {
    return Math.abs(evalPolynomial(p,x)) <= TOLERANCE
}

// evaluate p(x)
export function evalPolynomial(p: Polynomial, x: number): number {
    let answer = 0
    for(let n = 0; n <= degree(p); n++) {
        answer += p[n] * Math.pow(x, n)
    }
    return answer
}
```

There is nothing too crazy going on here. `isZero` just evaluates the polynomial and checks whether it's very close to zero. We don't expect it to be exactly zero because of rounding errors, but you can certainly make the tolerance lower than what we have here. `evalPolynomial` computes the sum of the terms, and each term is its coefficient multiplied by the input to the power of the degree of that term, `n`.

Now, for the last big piece, `endZeros`. Let's look at that graph again:

<iframe src="https://www.desmos.com/calculator/0wvhj6rha8?embed" width="500" height="500" style="border: 1px solid #ccc" frameborder=0></iframe>

We handled zeros on and between the zeros of the derivative. Now, we have to check for zeros outside of the zeros of the derivative. One can occur between the negative infinity and the minimum or leftmost zero (to the left of the first blue dot), and one can occur between the maximum or rightmost zero and positive infinity (to the right of the last blue dot). We can detect whether there will be a zero by examining the values of the polynomial at these derivative zeros and the polynomial's end behavior, which is whether it approaches positive or negative infinity as we go off to the left or right.

In our graph, the last dot is negative and the polynomial increases to the right. Since there are no more zeros of the derivative to the right, the polynomial can't change direction, so it must continue increasing forever, which means it will eventually reach zero if we continue to the right. However, on the left side, the leftmost blue dot is at zero and the polynomial increases to the left. The graph will just go away from zero forever to the left of the first dot. This means there are no zeros to the left of the blue dots. In general, if and only if the polynomial is heading towards zero beyond the zeros of the derivative, there will be a zero.

Now let's implement this:

```ts
// find zeros outside of the derivative zeros
function endZeros(p: Polynomial, derivativeZeros: number[]): number[] {
    if (derivativeZeros.length === 0) {
        return []
    } else {
        const zeros = []
        const minZero = derivativeZeros[0]
        const changeToLeft = evalPolynomial(p, minZero - 1) - evalPolynomial(p, minZero)
        const hasLeftZero = changeToLeft * evalPolynomial(p, minZero) < 0
        if (hasLeftZero) {
            zeros.push(findEndZero(p, minZero, -1))
        }
        const maxZero = derivativeZeros[derivativeZeros.length - 1]
        const changeToRight = evalPolynomial(p, maxZero + 1) - evalPolynomial(p, maxZero)
        const hasRightZero = changeToRight * evalPolynomial(p, maxZero) < 0
        if (hasRightZero) {
            zeros.push(findEndZero(p, maxZero, 1))
        }
        return zeros
    }
}
```

Since outside of the zeros of the derivative, the polynomial can't change direction, we just need to step a little bit past the zero to see which direction the polynomial will head in. That's what `changeToLeft` and `changeToRight` measure. To determine whether there is a zero, we compare the direction the polynomial is heading in (increasing vs decreasing) to the value of the polynomial to determine whether the polynomial is heading towards zero. In our example graph, if we're looking at the rightmost blue dot, the value of the polynomial is negative, and the `changeToRight` is positive. Since the polynomial is negative, it's increasing to the right, and it can't change direction, it must eventually hit zero! This is obvious from our graph because we can literally see it hit zero, but it is also true in general. To compute this, we multiply the value by the change and check whether that's negative.

On the left side of our graph, the polynomial has value zero and our change to the left is positive, so the product will be zero, which is not negative, indicating that there is no zero to the left. Nice!

When we determine that there is a zero beyond the zeros of the derivative, we use `findEndZero`, which takes in an \\(x\\)-value to start at and a direction to look in (negative for the left and positive for the right), and finds a zero in that direction.

We will implement this by taking steps of exponentially increasing size in the given direction until we see a sign change and then doing our binary search operation on the resulting interval:

```ts
// find a zero beyond x in the given direction
function findEndZero(p: Polynomial, x: number, direction: number): number {
    const otherX = findSignChange(p, x, direction)
    return findZeroBetween(p, x, otherX)
}

// find an x-value in the given direction such that there is a sign change between x and that value
function findSignChange(p: Polynomial, x: number, direction: number): number {
    let step = direction
    let px = evalPolynomial(p, x)
    let otherX, isSignChange
    do {
        otherX = x + step
        isSignChange = px * evalPolynomial(p, otherX) < 0
        step *= 2
    } while (!isSignChange)
    return otherX
}
```

And there we have it! We have implemented a nice little algorithm for finding the zeros of a polynomial.

I love how elegant the core idea is here. It's so nicely recursive, it involves derivatives, which I love, and it fits in under 150 lines of code. Beautiful!

The full source code can be found [here](https://github.com/quasarbright/recursive-polynomial-solver-for-blog/blob/master/src/index.ts).

<!-- TODO say array instead of list -->
