# Natural Transformation

 * Given a category C and a category D.
 * Given a functor F from C to D.
 * Given a functor G from C to D.
 
There is a natural transformation from F to G if, for every object X in category C, there is an endomorphism from F(X) to G(X).
[Paul: Two points:
1. The word "endomorphism" usually means a morphism from an object to itself.  In this case F(X) and G(X) are generally distinct, so you would just have a "morphism" from F(X) to G(X) rather than an "endomorphism".
2. Not just any collection of morphisms F(X) -> G(X) (ranging over objects X in C) determines a natural transformation: the collection must also be compatible with the morphisms of C.  More on that below.]
[Response:
1. So the natural transformation can be between different categories? Both F and G are defined as being mappings to category D, so the mapping between them must be an endomorphism.
2. So it must preserve morphisms. That means that the mapping between F(x) -> G(X) must be a functor?]

This means that every object X in category C must have a morphism between the corresponding objects in category D, after the two functors have been applied. That is a morphism from F(X) to G(X).

This does not establish that a morphism from G(X) to F(X) exists. It would only occur if the morphism were an isomorphism, which would make the natural transformation a natural isomorphism.
[Paul: This is indeed an important point.  I'll only add that there could be a morphism G(X) -> F(X) which is not the inverse of the given morphism F(X) -> G(X), so even if there is a natural transformation D -> C it is not necessarily true that the original one C -> D is an isomorphism.  But in any case, the point you're making is correct: a natural transformation only gives you morphisms in one direction.]

This is fine because if functor F is a bijection and functor G is a constant function then the morphism from F(X) to G(X) cannot be an isomorphism if there is more than one object X in category C.
All that is required is a structure preserving (set of morphisms?) (or must it be a single morphism?) from each object F(X) to G(X).

An example of this in haskell is Maybe.

 * The functor F(X) is Just.
 * The functor G(X) is Nothing.
 * The morphism between all F(X) and G(X) is Nothing (see [this issue though](https://github.com/matthewfranglen/category-set-graph/issues/1))

[Paul: Let me translate this example into my language to see if I understand it properly.  The categories C and D are both *Set*, the functor F: *Set* -> *Set* is the identity (sends each object to itself and each morphism to itself), and the functor G: *Set* -> *Set* sends each set to the empty set and each function between sets to the empty function from the empty set to itself.  You propose to construct a natural transformation F -> G.

However, this requires specifying, for each set X, a function from X to the empty set.  Unfortunately in standard set theory no such function can exist if X is nonempty - see the accepted answer to this question for a good explanation: https://math.stackexchange.com/questions/789123/why-is-there-no-function-with-a-nonempty-domain-and-an-empty-range (I'm not sure how this lines up with Haskell syntax).

However, for every set X there is a unique function (the "empty function") from the empty set to X, and indeed this can be used to construct a natural transformation from G to F.  (As mentioned above one must check compatibility with morphisms in C, but it works out fairly easily).]

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

[Paul: Note: the edge set E is a subset of V x V, not necessarily all of V x V.  Indeed, the edge set is all of V x V if and only if G is the complete directed graph on the vertex set V.

Actually, if one wants to consider directed multigraphs (multiple edges between the same pair of vertices are allowed) then you can't really even think of E as a subset of V x V.  Instead you think of E as just an abstract set equipped with a map E -> V x V which sends every edge e to its source and target.  This fits well with the functorial definition of a graph.]

```
F(a) -> V x V
F(b) -> V
F(s) -> fst
F(t) -> snd
```

(note: I believe that this definition requires that every vertex in the graph be both the source of an edge and destination of an edge)
[Paul: This is related to a concern you expressed on slack: that the construction wouldn't work for DAG's or graphs with isolated vertices.  But everything is fine: the key point is that the functions F(s) and F(t) from the edge set to the vertex set need not be surjective.  I'll illustrate by means of an example.

Consider the graph whose vertext set is V = {1,2,3,4} and whose edge set is E = {e=(1,2), f=(2,3)}.  So this is the path graph on the vertices 1,2,3 (a DAG) together with the isolated vertex 4.  Let us construct the functor X -> *Set* which corresponds to this graph.

As usual we set F(a) = E and F(b) = V.  Define F(s): F(a) -> F(b) by F(s)(e) = 1 and F(s)(f) = 2, and define F(t): F(a) -> F(b) by F(t)(e) = 2 and F(t)(f) = 3. The vertices 3 and 4 do not lie in the range of F(s), and the vertices 1 and 4 do not lie in the range of F(t), but that's ok - these are still well-defined functions.]

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
[Paul: You're almost there!  But as mentioned above you also have to check that the natural transformation is compatible with morphisms in X.  You can read about this in my post (https://pwsiegel.github.io/category-theory/graphs/), but let me clarify a little more in the context of this specific example.

As you pointed out, a graph homomorphism gives us a morphism η(a): F(a) -> G(a) and a morphism η(b): F(b) -> G(b).  But these can't be just any old pair of functions between vertex sets and edge sets; part of the definition of graph homomorphism is the statement that if e is an edge between vertices v and w then η(a)(e) is an edge between vertices η(b)(v) and η(b)(w).

So let's review the compatibility part of the definition of natural transformation.  The structure of the functor F includes morphisms F(s): F(a) -> F(b) and F(t): F(a) -> F(b) and similarly for G.  Focusing first on the morphism s, we have two distinct ways to use η and s to get from F(a) to G(b):

* The composition G(s) η(a) - first apply η(a) to get from F(a) to G(a), then apply G(s) to get from G(a) to G(b).
* The composition η(b) F(s) - first apply F(s) to get from F(a) to F(b), then apply η(b) to get from F(b) to G(b).

You must check that these two compositions are equal.  I'll leave you to it!  

> There is a natural transformation from F to G if, for every object X in category C, there is an endomorphism from F(X) to G(X).
