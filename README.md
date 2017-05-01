# category-set-graph

Consider the category X containing two objects a and b
 * there is a morphism from a to b
 * and a morphism from b to a

Consider also the category **Set** whose objects are sets and whose morphisms are functions between sets

Show that a graph is the same thing as a functor from X to **Set**.

I.E.
 * if i give you a graph you give me the functor
 * and if i give you a functor you tell me what the graph is

## Definitions

https://en.wikipedia.org/wiki/Category_of_sets
https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)
https://en.wikipedia.org/wiki/Graph_(topology)
https://en.wikipedia.org/wiki/Functor
https://en.wikipedia.org/wiki/Homomorphism

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


