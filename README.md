# Lambda Calculus

## Notes

An expression in lambda calculus can be:

1. variable: x
2. lambda abstraction (function): λx.x (where . separates the function argument and body)
3. function application: x y

Each lambda takes a single argument and multiple arguments are obtained by nesting lambdas (λx.(λy. x y))

Annonymous functions (λ v . e)

The identity functions (λ x . x)

# Substitution 

Computation in the lambda calculus takes place by substitution.  There are two main rules:

a-reduction (renaming bound variables): lx.E => ly.E[x/y], where E[x/y] denotes the result of replacing all free occurrences of x in E by y, provided y would not be captured by a ly already in E.  Example:
OK: lx.ly.xy => lz.ly.zy   (rename x to z, nothing bad happened)
not OK: lx.ly.xy => ly.ly.yy   (rename x to y, y was captured)
b-reduction (substitution rule): (lx.E  F) => E[x/F], where E[x/F] denotes the result of replacing all free occurrences of x in E by F. Before doing this, bound variables in E are renamed by a-reduction if necessary to avoid capturing free variables in F.  Example:
OK: (lx.ly.xy  lz.z) => ly.(lz.z  y)
not OK: (lx.ly.xy  lz.yz) => ly.(lz.yz  y)   (y was captured)
but if we rename the bound y first, it's OK:
(lx.ly.xy  lz.yz) => (lx.la.xa  lz.yz) => la.(lz.yz  a)
Examples in Scheme:

(lambda (x) (+ x 3)) is equivalent to (lambda (y) (+ y 3)).
This is just the substitution rule.  ((lambda (x) (+ x 3)) 7) => (+ 7 3).
An a- or b-reduction step can be performed at any time to any subterm of a lambda term.  Write E => F if E goes to F in some finite number of a- or b-reduction steps.


## Numbers

0 = λs.λz. z

1 = λs.λz. s z

2 = λs.λz. s (s z)

## Booleans

true = λt.λf.t

false = λt.λf.f

```scheme
(lambda (x) x)
```

eval  : Expression * Environment -> Value
apply : Value * Value -> Value

Environment = Variable -> Value

Value       = Closure

Closure     = Lambda * Environment
