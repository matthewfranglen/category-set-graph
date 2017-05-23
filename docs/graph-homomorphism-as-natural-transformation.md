# Natural Transformation

Given a category C and a category D.
Given a functor F from C to D.
Given a functor G from C to D.
There is a natural transformation from F to G if, for every object X in category C, there is an endomorphism from F(X) to G(X).

This means that every object X in category C must have a morphism between the corresponding objects in category D, after the two functors have been applied. That is a morphism from F(X) to G(X).

This does not establish that a morphism from G(X) to F(X) exists. It would only occur if the morphism were an isomorphism, which would make the natural transformation a natural isomorphism.

This is fine because if functor F is a bijection and functor G is a constant function then the morphism from F(X) to G(X) cannot be an isomorphism if there is more than one object X in category C.
All that is required is a structure preserving (set of morphisms?) (or must it be a single morphism?) from each object F(X) to G(X).

An example of this in haskell is Maybe.

The functor F(X) is Just.
The functor G(X) is Nothing.
The morphism between all F(X) and G(X) is Nothing.

```haskell
f x = Just x
g _ = Nothing
η _ = Nothing
```

Testing it:

```haskell
ghci ➜ f 1
Just 1
ghci ➜ g 1
Nothing
ghci ➜ (η . f) 1
Nothing
```

# Graph Homomorphism

# Equivalence
