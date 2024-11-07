    Title: Computing Fibonacci Numbers in Logarithmic Time
    Date: 2024-08-09T13:15:59
    Tags: math, algorithms, dynamic-programming, JavaScript

In this post, we're going to implement an algorithm for computing the \\(n\\)th fibonacci number. We'll gradually optimize it and eventually end up with an algorithm that takes \\(O(\log n)\\) steps, which is much better than the \\(n\\) steps that most implementations take. I'll assume some familiarity with complexity analysis. If you know big O notation, you'll be fine.

<!-- more -->

# A Recursive Algorithm

The fibonacci sequence is defined recursively:

$$ F_0 = 0 $$
$$ F_1 = 1 $$
$$ F_n = F_{n-1} + F_{n-2} $$

Its elements are \\(0,1,1,2,3,5,8,13, \ldots\\). Each term is the sum of the previous two. Let's translate this definition directly into code:

```ts
function fibRecursive(n: number): number {
  if (n === 0) {
    return 0
  } else if (n === 1) {
    return 1
  } else {
    return fibRecursive(n - 1) + fibRecursive(n - 2)
  }
}
```

This works, but it's pretty inefficient. We're duplicating a lot of work. To see just how bad this is, let's think about each recursive call. Here is the call tree:

```
+ fibRecursive(4)
+--+ fibRecursive(3)
|  +--+ fibRecursive(2)
|  |  +--- fibRecursive(1)
|  |  +--- fibRecursive(0)
|  +--- fibRecursive(1)
|
+--+ fibRecursive(2)
   +--- fibRecursive(1)
   +--- fibRecursive(0)
```

We end up doing all the work of `fibRecursive(2)` twice. For bigger inputs, there is even more duplicated.

How many steps will this algorithm perform? The base cases only take 1 step, but the recursive case takes however many steps `fibRecursive(n-1)` takes, plus however many steps `fibRecursive(n-2)` takes, plus one to add the results together. Sound familiar? The number of steps this algorithm takes is basically \\(O(F_n)\\)! It's a little worse because of that plus one though. This is almost as bad as exponential runtime, which is really slow. We can do better.

## Dynamic Programming

Our recursive algorithm is slow because we re-compute the same things over and over again. What if we just remember the answer each time we compute a fibonacci number and then use our stored answer the second time around? This is caled memoization, which is a technique of dynamic programming.

```ts
const savedFibs = new Map<number, number>()
function fibMemo(n: number): number {
  if (n === 0) {
    return 0
  } else if (n === 1) {
    return 1
  } else if (savedFibs.has(n)) {
    return savedFibs.get(n)!
  } else {
    const result = fibMemo(n - 1) + fibMemo(n - 2)
    savedFibs.set(n, result)
    return result
  }
}
```

Before we make a recursive call, we check if we've already computed this fibonacci number, and if we have, just use the stored result. Otherwise, actually make the recursive call and store the result in `savedFibs` so we don't compute it again. How many steps does this take? We still compute every fibonacci number from \\(0 \ldots n\\), but only once, so it's \\(O(n)\\), which is not bad! But there's still a problem.

Although this algorithm is faster, we're using up memory storing all of these fibonacci numbers. We're using \\(O(n)\\) space since we store about \\(n\\) numbers in `savedFibs` when we compute the \\(n\\)th fibonacci number. Also, since our algorithm is recursive, we're taking up stack space while we make recursive calls. This is \\(O(n)\\) in both the original recursive algorithm and the memoized algorithm since we're going to have a call stack \\(n\\) calls deep at some point. Can we do better?

Let's think about how `savedFibs` gets built up by running through `fibMemo(4)`. `fibMemo(4)` will call `fibMemo(3)`, which will call `fibMemo(2)`, which will call `fibMemo(1)` and `fibMemo(0)`, which will immediately return. We'll then compute that `fibMemo(2)` is 1 after adding the recursive calls, store that in `savedFibs`, and return to `fibMemo(3)`. Then, we'll compute `fibMemo(1)`, which will immediately return. Next, we'll compute that `fibMemo(3)` is 2 after adding up the recursive calls, store that in `savedFibs`, and return to `fibMemo(4)`. Then, we'll recursively call `fibMemo(2)` which will immediately return since its result is saved, we'll add up the recursive calls to determine that `fibMemo(4)` is 3, store that in `savedFibs`, and then finally return.

In that example, `fibMemo(2) = 1` was stored, then `fibMemo(3) = 2`, then `fibMemo(4) = 3`. Although the computation is top-down with big inputs recursively calling on small inputs, our saved results end up being build bottom-up with the results of small inputs being stored first. When we call `fibMemo(n)`, we'll end up storing `fibMemo(2)`, then `fibMemo(3)`, then `fibMemo(4)`, and so on until we get up to `n`. What if we just built this up directly instead of doing recursive calls?

```ts
function fibMemoBottomUp(n: number): number {
  const savedFibs: number[] = [0, 1]
  for (let i = 2; i <= n; i++) {
    savedFibs.push(savedFibs[i-1] + savedFibs[i-2])
  }
  return savedFibs[n]
}
```

We're no longer recursive, but we still take up linear space because we're building an array with all of the fibonacci numbers. But we don't really need the whole array. We only ever need the last two elements, so let's only store those:

```ts
function fibIterative(n: number): number {
  if (n === 0) {
    return 0
  }
  // fib(i-2)
  let prev = 0
  // fib(i-1)
  let current = 1
  for (let i = 2; i <= n; i++) {
    // fib(i)
    const next = prev + current
    prev = current
    current = next
  }
  return current
}
```

This is even better. Now we have linear runtime and constant space.

Let's review what just happened. We started out with a simple recursive implementation that ended up being slow from duplicate computaitons. Then, we did some memoization to make it faster. Next, we converted our top-down memoization to bottom up. Finally, we realized that with our bottom-up method, we could perform an optimization to make our algorithm even more efficient. This pattern of recursive, memoized, bottom up, then optimized is very common in dynamic programming.

Linear time is good, but what if I told you that it's possible to compute the \\(n\\)th fibonacci number in \\(O(\log n)\\) steps? Sounds impossible, right? Don't we need to compute them in sequence? How could you do that faster than \\(O(n)\\)?

# The Matrix

Donald E. Knuth showed that the \\(n\\)th fibonacci number can be calculated with matrix multiplication:

$$ \begin{bmatrix}
F_{n+1} & F_n\\\\
F_n & F_{n-1}
\end{bmatrix}
=
\begin{bmatrix}
1 & 1\\\\
1 & 0
\end{bmatrix}
^ n $$

To get an intuition for why this works, let's see the recursive step in action:

$$
\begin{bmatrix}
F_{n+1} & F_n\\\\
F_n & F_{n-1}
\end{bmatrix}
\ 
\begin{bmatrix}
1 & 1\\\\
1 & 0
\end{bmatrix} =
\begin{bmatrix}
(1F_{n+1} + 1F_n) & (1F_{n+1} + 0F_n)\\\\
(1F_n + 1F_{n-1}) & (1F_n + 0F_{n-1})
\end{bmatrix} =
\begin{bmatrix}
F_{n+2} & F_{n+1}\\\\
F_{n+1} & F_{n}
\end{bmatrix}
$$

Multiplying by the matrix gives us the next step in the sequence. This matrix of zeros and ones causes matrix multiplication to add the numbers up in just the right way to compute fibonacci numbers.

This is cool, but it doesn't help us beat linear runtime since exponentiation to the power of \\(n\\) requires \\(O(n)\\) multiplications. Or does it?

## Repeated Squaring

Let's say I want to compute \\(x^{16}\\). Instead of multiplying \\(x\\) with itself 16 times, we can just square it 4 times, which is 4 multiplications:

$$ x^{16} = \left(\left(\left(x^2\right)^2\right)^2\right)^2 $$

This little trick works nicely for exponents that are powers of 2, but what about other numbers like 10?

$$ x^{10} = x^2 x^8 = x^2 \left(\left(x^2\right)^2\right)^2 $$

That's 5 multiplications: 3 for \\(x^8\\), 1 for \\(x^2\\), and 1 to multiply them together. But if we avoid re-computing \\(x^2\\), we can get it down to 4. We can always avoid these re-computations by remembering smaller squarings as we go:

$$ x^7 = x x^2 x^4 $$
$$ y = x^2 $$
$$ z = y^2 $$
$$ x^7 = xyz $$

Sound familiar? This is another bottom-up approach to avoid duplicate computation.

This only has 4 multiplications.

Can we always split a number up like this? Yes! Every number can be broken down into a sum of powers of two. That's exactly what a number's binary representation is:

$$ 12 = 1 \cdot 8 + 1 \cdot 4 + 0 \cdot 2 + 0 \cdot 1 = 1100_2 $$

In general, the number of multiplications necessary for \\(x^n\\) with our repeated squaring algorithm is \\(O(\log n)\\) (specifically \\(\log_2\\)) since that's how many squarings we need to perform, and the number of multiplications to combine squarings is similar.

Let's implement this for numbers before we do matrices:

```ts
function pow(x: number, n: number) {
  let product = 1
  let square = x
  while (n > 0) {
    const remainder = n % 2
    if (remainder === 1) {
      product *= square
    }
    n = Math.floor(n / 2)
    square = square * square
  }
  return product
}
```

We're essentially converting the number to binary, from least significant to most significant digits (right to left). If there is a 1-digit at that point in the number's binary representation, we multiply the accumulated product by the current squaring of \\(x\\). In each iteration, we square again. The `square` variable stores \\(x\\), then \\(x^2\\), then \\((x^2)^2\\), and so on. This allows us to avoid re-computing squares. This is a bottom-up, iterative algorithm similar to our last fibonacci algorithm.

Now let's implement `pow` for matrices:

```ts
function matPow(M: number[][], n: number) {
  // assume M is a square matrix
  const size = M.length
  let product = identity(size)
  let square = M
  while (n > 0) {
    const remainder = n % 2
    if (remainder === 1) {
      product = matMul(product, square)
    }
    n = Math.floor(n / 2)
    square = matMul(square, square)
  }
  return product
}

function matMul(A: number[][], B: number[][]) {
  // assume A,B are square matrices of the same size to keep things simple
  const size = A.length
  const product = []
  for (let r = 0; r < size; r++) {
    const row = []
    for (let c = 0; c < size; c++) {
      let dot = 0
      for (let i = 0; i < size; i++) {
        dot += A[r][i] * B[i][c]
      }
      row.push(dot)
    }
    product.push(row)
  }
  return product
}

// Identity matrix
function identity(size: number): number[][] {
  const M = []
  for(let r = 0; r < size; r++) {
    const row = Array(size).fill(0)
    row[r] = 1
    M.push(row)
  }
  return M
}
```

It's pretty much the same thing, except we use matrix operations instead of standard numeric ones.

# Putting it All Together

Now, we can use our efficient exponentiation algorithm to raise our fibonacci matrix to the \\(n\\)th power:

```ts
const M = [
    [1,1],
    [1,0]
]

function fibMat(n: number) {
  return matPow(M, n)[0][1]
}
```

That's it! Since `matPow` has \\(O(\log n)\\) runtime and constant space, so does our new and improved fibonacci algorithm.

