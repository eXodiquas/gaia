# Gaia

Gaia allows easy usage of genetic and evolutionary algorithms.

## Current State

Currently only basic calculations are possible. Only real variables can be used as genes (at least in a somewhat comfortable manner).

## Example Usage

```lisp
;; Calculate the maximum of f(x) = -x² + 100. Which should be 100. :P

(let ((p (new-population-initialized 100 (lambda (x) (apply (lambda (y) (+ (- (* y y)) 100)) x)) :gene-amount 1)))
    (loop for count upto 100 do
      (setf p (next-generation p)))
    (population-fitness p))

```
The result of this calculation is ~99.9999, which is close enough. At least we didn't have to do the algebra. AMIRIGHT?

This chunky looking piece of code first binds a pre-initialized population with fitness function f(x) = -x² + 100 to the local variable p. :gene-amount tells us how many genes an individual should have. We only want 1 gene per individual, otherwise it would not fit with our fitness function.
A pre-initialized population runs on :real values with a mutation rate of 5%.
Then we loop 100 times (arbitrary number, but we have to stop somewhere).
Each loop iteration we set p to the next generation.
After 100 steps we return the average fitness of the population. We could also get the fittest individual and return its fitness.

## Next Steps

- [X] Individuals
- [X] Populations
- [X] Crossover
- [X] Mutation

Now that basic evaluation is possible I want to implement more strategies.

[] Tournament Selection
- [] Stochastic Uniform Sampling Selection
- [] Bit-Strings as gene type for individuals
- [] Mutation for integers
- [] Mutation for bit-strings
- [] Make parameter accessible (like for example, what proportion of the N best individuals should be used for uniform recombination)
- [] Macro to not always have to write (lambda (x) (apply #'fun x)) as fitness function
- [] Make it easier to setup maximizing or minimizing algorithms
