# Traitor

**Traitor**, *noun*, possibly from old French or somewhere. Definition:

1. A person who double-crosses or otherwise does harm to a trusting party.

2. An uncommon (some even say made-up) term for a person or package which
bestows traits on (or *betrays*) a programming language.

## Overview

The *Traitor.jl* is a naive (and potentially evil) attempt to create a
convenient framework for trait-based dispatch in the Julia programming language.
Traits provide an alternative to *single-inheritance multiple-dispatch*,
which would allow for complex combinations of object properties to be tested
at method dispatch.

Currently it is definitely a work in progress, but it supports the features
discussed in the following sections.

**Final warning: if you use this package, it might not actually work yet!**.

### Our expectations of a traits type system

Trait types are organized into the type system. An abstract type defines the
trait class, while *concrete* subtypes of them define instances of that trait.

For example:

```julia
abstract Size

immutable Big <: Size; end
immutable Medium <: Size; end
immutable Small <: Size; end
```

Types are annotated with traits *post-hoc* by returning the appropriate trait
*instance* from the constructor of the abstract trait class (see `Union{}`,
below). So we could have:

```julia
Size(::Union{Int32,Int64}) = Small() # Note the right is an instance, not a type
Size(::BigInt) = Big()
Size(::Int128) = Medium()
```

### The `@traitor` macro

The `@traitor` macro is used to define a function which will use trait-based
dispatch. Currently, it suffers from several limitations. Chiefly, `@traitor`
functions are not very compatible with standard generic functions, so it is best
that a function only takes `@traitor` method definitions. The traits to
dispatch upon are given by a flexible extra set of `::` operators.

For a quick example, we allow for instance:
```julia
@traitor function howbig(x::Any::Big)
    "Huge!"
end

@traitor function howbig(x::::Medium)
    "So-so"
end

@traitor function howbig(x::Integer::Small)
    "Teensy..."
end
```

### Computable traits

To our utter dismay, it turns out that traits can be computed. Watch out for
this slippery code below:

```julia
"The `Treason` module implements stuff that Jeff and Stefan won't like."
module Treason
using Traitor

abstract Mutability
immutable Immutable; end
immutable Mutable; end

Mutability(x) = isimmutable(x) ? Immutable() : Mutable()

"Boring, old definition..."
function map(f, x::::Mutable)
    out = similar(x)
    for i = 1:length(x)
        out[i] = f(x[i])
    end
    return out
end

"Cool, but please optimize me onto the stack, oh Julia compiler gods"
function map(f, x::::Immutable)
    out = Ref(similar(x)) # Get one on the heap
    for i = 1:length(x)
        out[i] = f(x[i]) # Elsewhere define setindex!(::RefValue{Tuple}), etc
    end
    return out.x # Copy it back to the stack
end

function setindex!{T <: Tuple}(x::RefValue{T}, val, i::Integer)
    # grab a pointer and set some memory (oh, and make sure val is converted to
    # correct type first, if you care about things like segfaults)
end

end # module
```

### Unions of traits

Trait constraints can be relaxed by writing `Union{}`s of trait types *of the
same trait class*. For example:
```julia
@traitor roughlyhowbig(x::::Big) = "Huge!"
@traitor roughlyhowbig(x::::Union{Medium,Small}) = "Smallish"
```

This is where our design decision to make traits defined by instances are
important - it allows us to leverage the existing dispatch on `::Union{}` signatures
which is more convenient than `::Type{Union{...}}` signatures. (PS - this will
probably switch back with our new generate function approach.)

### Intersection of traits

Unfortunately, this is where our traitorous mutiny against single inheritance
currently falls down into a pile of sticks. Although somewhat useful as-is, it
is clear that interesting intersections of traits is what we are looking for.
If you made it this far, and have any ideas, please discuss or contribute!

The simplest approach could be to make trait-based functions static in which traits
they know. There could be a pre-defintion along the lines of

```julia
@trait function f::((Size, Odor), (), (Size,))
```
where the right defines which traits apply to which arguments (the first allows
both `Size` and `Odor`, while the second is traitless™, and the third dispatches
on `Size` only).

The most complete approach would intercept dispatch (with a generated function) and
store it's own method table (including trait annotation) and do our own dispatch
algorithm. There is WIP in *TraitorFunction.jl* to do this, by first allowing
multiple-dispatch to run first, and then using our own method lookup. In fact,
this kind of composed-dispatch algorithm seems very inviting - we don't involve
multiple-inheritance in the standard Julia approach, and conversely the trait
dispatch (allowing multiple-inheritance) is easy since the traits themselves are
quite simple.

Unfortunately, we haven't figured out a way to make these interact
with generic functions, but it *might* provide a playground where you can define
new "traitor" functions in order to prototype where to go in Base Julia.

## Acknowledgements

This is joint work by Chris Foster (**@c42f**) and Andy Ferris (**@andyferris**).
