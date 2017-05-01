# category-set-graph

Consider the category X containing two objects a and b
 * there is a morphism from a to b
 * and a morphism from b to a

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

## How is category Set a graph

The individual sets are the vertexes and the morphisms are the edges.
The graph this represents is (nearly) complete. The empty set has no morphisms to it.

## How is a function from a singleton to category Set a graph

A function from a singleton is a constant function.
So a function from a singleton to category Set just returns category Set.

() = singleton
S = category Set
() -> S

This requires that I return the entire category when provided a single value.

If a more explicit graph form is required then it needs to define the vertexes, and the edges between them.
Then a mapping between those vertexes and the sets can be defined.
Finally a mapping between the edges and the morphisms can be defined.
I'm not sure how the `f` (in `(f, A, B)` from wikipedia) would be resolved.

This graph could be unidirectional.

## How can category X be mapped to category Set

The mapping between category X and category Set must retain the structure that
category X has. Since the structure in category X is that a and b have an
isomorphism between them that means that the two values produced by the mapping
must have an isomorphism between them.

One way to preserve this is to map both values in category X to the 

## Definitions

https://en.wikipedia.org/wiki/Category_of_sets
https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)
https://en.wikipedia.org/wiki/Graph_(topology)
https://en.wikipedia.org/wiki/Functor
https://en.wikipedia.org/wiki/Homomorphism
https://en.wikipedia.org/wiki/Constant_function

## Thoughts

Need to define a graph
Need to show how the category Set is a graph
Need to show how a functor can produce a graph, or how it is a graph
Need to show how a functor can preserve the existing morphisms between a and b.

If the category X had only a single value then a functor could be constant.
That functor could then just define the graph vertices as the individual sets and the graph edges as the morphisms between the sets.
That seems pretty trivial so the complexity of the question must lie in the fact that there are two values in category X.

The two values in category X are isomorphic.
A functor between categories must preserve the structure.
This means that if the functor between X and Set maps a and b to different sets then there must be an isomorphic relationship between the sets.
This isomorphic relationship can be the bi directional edge of the graph.


