# Natural Transformation

 * Given a category C and a category D.
 * Given a functor F from C to D.
 * Given a functor G from C to D.
 
There is a natural transformation from F to G if, for every object X in category C, there is an endomorphism from F(X) to G(X).

This means that every object X in category C must have a morphism between the corresponding objects in category D, after the two functors have been applied. That is a morphism from F(X) to G(X).

This does not establish that a morphism from G(X) to F(X) exists. It would only occur if the morphism were an isomorphism, which would make the natural transformation a natural isomorphism.

This is fine because if functor F is a bijection and functor G is a constant function then the morphism from F(X) to G(X) cannot be an isomorphism if there is more than one object X in category C.
All that is required is a structure preserving (set of morphisms?) (or must it be a single morphism?) from each object F(X) to G(X).

An example of this in haskell is Maybe.

 * The functor F(X) is Just.
 * The functor G(X) is Nothing.
 * The morphism between all F(X) and G(X) is Nothing (see [this issue though](https://github.com/matthewfranglen/category-set-graph/issues/1))

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

A graph homomorphism is a mapping between graphs which respects their structure. Wikipedia:

> it is a function between the vertex sets of two graphs that maps adjacent vertices to adjacent vertices.

https://en.wikipedia.org/wiki/Graph_homomorphism

# Equivalence

Given the definitions from pauls previous test, we have:

 * Given the category X which contains two objects, a and b.
 * Given the category Set.
 * Given the two endomorphisms from a to b, s and t.
 * Given the functor from X to Set, F.

We then equate this to a graph by stating that:

 * A graph consists of vertexes V.
 * A graph consists of edges E, which are defined as products of vertexes V x V.

```
F(a) -> V x V
F(b) -> V
F(s) -> fst
F(t) -> snd
```

(note: I believe that this definition requires that every vertex in the graph be both the source of an edge and destination of an edge)

We then want to demonstrate that two separate graphs which have a graph homomorphism between them are equivalent to a natural transformation.

So the graph homomorphism between them is
 * A mapping between the vertex sets
 * Which maps adjacent vertices to adjacent vertices (i.e. it maps edges too)

This means that given:
 * A mapping from category X to category Set, F
 * A mapping from category X to category Set, G

Then the two sets of graph edges are F(a) and G(a), and the two sets of graph vertices are F(b) and G(b).
Therefore the graph homomorphism is a mapping from F(a) to G(a) _and_ the mapping from F(b) to G(b).

Since this graph homomorphism is defined as a mapping between functors for every value in category C it therefore meets the definition of a natural transformation.

> There is a natural transformation from F to G if, for every object X in category C, there is an endomorphism from F(X) to G(X).
