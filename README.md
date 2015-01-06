# Lambda Calculus

## Notes

An expression in lambda calculus can be:

1. variable: x
2. lambda abstraction (function): λx.x (where . separates the function argument and body)
3. function application: x y

Each lambda takes a single argument and multiple arguments are obtained by nesting lambdas (λx.(λy. x y))

Annonymous functions (λ v . e)

The identity functions (λ x . x)

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
