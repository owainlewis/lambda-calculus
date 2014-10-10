# Lambda Calculus

## Notes

Annonymous functions (λ v . e)

The identity functions (λ x . x)

```scheme
(lambda (x) x)
```

eval  : Expression * Environment -> Value
apply : Value * Value -> Value

Environment = Variable -> Value
Value       = Closure
Closure     = Lambda * Environment
