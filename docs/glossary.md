https://en.wikipedia.org/wiki/Glossary_of_category_theory
https://en.wikipedia.org/wiki/Mathematical_notation
https://en.wikipedia.org/wiki/List_of_mathematical_symbols

### Object

A category is a collection of objects.

This definition doesn't actually define what an object _is_.
I'm not sure it matters.

### Arrow

In the blog post it describes them as:

> Usually these “arrows” are functions, but in general they don’t have to be.

That would mean that they are not morphisms or functors as they don't have to preserve structure (they are the structure).
They are always within the category.

### Category

A category is a collection of objects and arrows between objects.

https://www.johndcook.com/blog/2017/03/16/natural-transformations/

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

In haskell I can define two categories by starting with the objects within them:

```haskell
data CatA = AA | AB
    deriving Show

data CatB = BA | BB
    deriving Show
```

I can then define some internal structure by defining the arrow f:

```haskell
f :: CatA -> CatA
f AA = BB
f BB = AA
```

If I define a functor from CatA to CatB, then it must preserve structure. So there must be an arrow equivalent to f in CatB:

```haskell
g :: CatB -> CatB
g BA = BB
g BB = BA
```

So given that I can define the functor from CatA to CatB:

```haskell
functor :: CatA -> CatB
functor AA = BA
functor AB = BB
```

Unfortunately at this point I need to be able to define a mapping from CatB to CatA to define the mapping from f to g automatically.
This is not part of the definition of a functor.
So it must be that you explicitly state it:

```haskell
functor' :: (CatA -> CatA) -> (CatB -> CatB)
functor' f = functor . f . inverse
    where inverse :: CatB -> CatA
          inverse BA = AA
          inverse BB = AB
```

### Natural Transformation

A mapping from one functor to another which preserves the arrows of the categories of the functors.
This is defined as a mapping of both the objects in the categories and the arrows in the categories.

https://en.wikipedia.org/wiki/Natural_transformation
https://www.johndcook.com/blog/2017/03/16/natural-transformations/
