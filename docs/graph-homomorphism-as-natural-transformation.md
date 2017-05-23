# Natural Transformation

Given a category C and a category D.
Given a functor f from C to D.
Given a functor g from C to D.
There is a natural transformation from f to g if there is an endomorphism from f(C) to g(C).

This means that every object in category C must have a morphism between the corresponding objects in category D, after the two functors have been applied. That is a morphism from f(C) to g(C).

This does not establish that a morphism from g(C) to f(C) exists. It would only occur if the morphism were an isomorphism, which would make the natural transformation a natural isomorphism.

This is fine because if functor f is a bijection and functor g is a constant function then the morphism from f(C) to g(C) cannot be an isomorphism if there is more than one object in category C. All that is required is a structure preserving (set of morphisms?) (or must it be a single morphism?) from each object f(C) to g(C).
