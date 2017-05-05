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

So if the category Set is a graph, then a function from a singleton to the category Set is equivalent to the category Set, and is thus equivalent to a graph.

## How is an arbitrary graph contained within category Set

The category Set is infinite?
If it is then any arbitrary graph can fit within it, by using some of the sets as the vertexes and then using (some of) the morphisms between those sets as the edges.

If a more explicit graph form is required then it needs to define the vertexes, and the edges between them.
Then a mapping between those vertexes and the sets can be defined.
Finally a mapping between the edges and the morphisms can be defined.
I'm not sure how the `f` (in `(f, A, B)` from wikipedia) would be resolved.

This graph could be unidirectional.

## How can category X be mapped to category Set

### Cheap Way

The mapping between category X and category Set must retain the structure that
category X has. Since the structure in category X is that a and b have an
isomorphism between them that means that the two values produced by the mapping
must have an isomorphism between them.

One way to preserve this is to map both values in category X to the same graph
within category Set. Then the isomorphism is identity. This would allow a
unidirectional graph, as the morphisms between the sets would be incidental to
the original structure of category X.

### Better Way?

Another way would be to:

 * Order the target sets arbitrarily
 * Define a mapping from X(a) -> Set(a) which includes the target sets. The morphisms between sets {x, y} are included if `x <= y`
 * Define a mapping from X(b) -> Set(b) which includes the target sets. The morphisms between sets {x, y} are included if `x >= y`
 * Define a mapping from Set(a) -> Set(b) where the morphisms between sets are flipped
 * Repeat this for Set(b) -> Set(a)

In this case the mapping between X and Set would be a bidirectional graph as both directions would be represented.
This solution feels a little bit better as it doesn't immediately dismiss one of the defining qualities of category X (that it has two values, not one).

## To define the graph given the functor

Just run it! (╭☞ ͡⎚ ͜ʖ ͡⎚)╭☞
A functor can be defined which transforms a subset of the category Set to a graph. It would:

 * Map the sets to unique vertexes
 * Map the morphisms to edges

That function can be composed with the provided function to produce the graph. It should work regardless of which specific value from category X was provided.

If the _better way_ was being taken then the edges that are defined should be bi directional.

## To define the functor given the graph

 * Map each vertex in the graph to a set.

### Cheap Way

 * Map each edge in the graph to the morphism between the sets.
 * Define a constant functor which returns the mapped sets and morphisms

### Better Way?

 * Order the sets
 * Define a mapping from X(a) -> Set(a) which includes the target sets. The morphisms between sets {x, y} are included if `x <= y`
 * Define a mapping from X(b) -> Set(b) which includes the target sets. The morphisms between sets {x, y} are included if `x >= y`
 * Define a functor which applies the appropriate mapping given the input value

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

## Problems

So I discussed this with Paul and he stated that this approach has problems. The problems are:

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
