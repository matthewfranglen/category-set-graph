https://en.wikipedia.org/wiki/Glossary_of_category_theory
https://en.wikipedia.org/wiki/Mathematical_notation
https://en.wikipedia.org/wiki/List_of_mathematical_symbols

### Morphism

A mapping from one category to another category which preserves structure.
They can be the same category.

This doesn't appear in the wikipedia glossary for category theory.
The [page for it](https://en.wikipedia.org/wiki/Morphism) states the following:

> In many fields of mathematics, morphism refers to a structure-preserving map from one mathematical structure to another. The notion of morphism recurs in much of contemporary mathematics. In set theory, morphisms are functions; in linear algebra, linear transformations; in group theory, group homomorphisms; in topology, continuous functions, and so on.
>
> In category theory, morphism is a broadly similar idea, but somewhat more abstract: the mathematical objects involved need not be sets, and the relationship between them may be something more general than a map.

### Endomorphism

A morphism from a category to itself.

### Isomorphism

A morphism which has an inverse morphism.
Applying one after the other is equivalent to the identity function.

Given _f: X → Y_ and _g: Y → X_ these form an isomorphism if _g ∘ f = id(X)_ and _f ∘ g = id(Y)_.

### Functor

A mapping from one category to another category which preserves morphisms.
I think that structure can be more general than morphisms so all functors are morphisms, but not all morphisms are functors.

https://en.wikipedia.org/wiki/Functor

### Natural Transformation

A mapping from one functor to another which preserves the morphisms of the categories of the functors.
This is defined as a mapping of both the objects in the categories and the morphisms in the categories.

To try to represent this in haskell

```haskell
η :: (a -> b) -> (a -> b)
```

https://en.wikipedia.org/wiki/Natural_transformation
