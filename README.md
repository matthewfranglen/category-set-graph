# category-set-graph

Consider the category X containing two objects a and b
 * there is a morphism from a to b
 * and another morphism from a to b

Consider also the category **Set** whose objects are sets and whose morphisms are functions between sets

Show that a graph is the same thing as a functor from X to **Set**.

I.E.
 * if i give you a graph you give me the functor
 * and if i give you a functor you tell me what the graph is

## What is a Graph

https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)

A graph consists of vertexes and edges.
A vertex is an object and an edge is a relationship between two objects.
An edge can be unidirectional, or bidirectional.

You can represent a graph as a list of the vertexes and a list of the edges. For example:

Vertexes: [1, 2, 3]
Edges: [{1, 2}, {2, 3}, {3, 1}] // {from, to}

For this I will take all edges to be unidirectional (so the existence of the edge from 1 to 2 does not mean that an edge from 2 to 1 exists).

## What is category Set

https://en.wikipedia.org/wiki/Category_of_sets

This contains all sets.
Every set has a morphism to every other set (except that there are no morphisms to the empty set).
The representation of the morphisms on wikipedia is {f, from, to}

## What is the minimal mapping

The minimal mapping that a function can provide is a value in category X to no value in category Set.
I have defined this as Lib.toNothing

The next most minimal mapping is a function from category X to the singleton set in category Set.
I have defined this as Lib.toSingleton

Are these functors?
I don't think they are.

## Functor

The haskell idea of a functor is something that provides fmap. I don't think that definition is applicable to this problem.

The mathematical definition of a functor is a mapping between two categories. Quoting:

    Let C and D be categories. A functor F from C to D is a mapping that:

        associates to each object X in C an object F ( X ) in D,
        associates to each morphism f : X → Y in C a morphism F ( f ) : F ( X ) → F ( Y ) in D such that the following two conditions hold:
                F ( id X ) = id F ( X ) for every object X in C,
                F ( g ∘ f ) = F ( g ) ∘ F ( f ) for all morphisms f : X → Y and g : Y → Z in C.

    That is, functors must preserve identity morphisms and composition of morphisms.

This seems a pretty reasonable definition, and means that:
(for this category X object A is denoted _Xa_, category X object B is denoted _Xb_).

 * toNothing _is not_ a functor
   because it doesn't map an object in category X to an object in category Set

 * toSingleton _is_ a functor
   because it maps `Xa -> ()` and `Xb -> ()`
   and the morphisms present between Xa and Xb are preserved as the `id` morphism.


## Definitions

https://en.wikipedia.org/wiki/Category_of_sets
https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)
https://en.wikipedia.org/wiki/Graph_(topology)
https://en.wikipedia.org/wiki/Functor
https://en.wikipedia.org/wiki/Homomorphism
https://en.wikipedia.org/wiki/Constant_function

## Thoughts

Need to define a graph
Need to define category Set

Need to define a functor from category X to category Set.

## Problems

My original approach was to define a graph in terms of category Set.
The sets were vertexes and the morphisms between sets were edges.

So I discussed this with Paul and he stated that the original approach has problems.
The problems are:

 * Morphisms _must_ compose, and composition produces a new morphism.
   If I have a morphism `a -> b` and a morphism `b -> c` then I can compose them to produce the morphism `a -> c`.

 * Edges do not compose.
   If I have an edge `a -> b` and an edge `b -> c` then there is not automatically an edge `a -> c`.
   You can traverse that path, it just passes through a vertex.
   A graph does not become complete when every vertex is reachable from every other vertex, it is complete when every vertex is directly connected via an edge.
   This is re-enforced by the fact that a connected graph is one where every vertex is reachable from every other vertex (i.e. there are two terms and so there is a difference).

The solution to this is the category Path, where the values are lists of
connected edges (e.g. `a -> b -> c` from the above). Here the composition of
two paths is merely the concatenation of the two lists of edges.

This then leads to another problem, which is that two different graphs may be
represented by the same category of paths. Paul did not have any examples.
(When he mentioned this I wondered if something like chirality would be an
example, I'm not sure though because the non-superposable quality relies on the
physical constraints of the bonds which don't have a relation to graph edges).
