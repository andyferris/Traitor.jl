# Traitor

**Traitor**, *noun*, possibly from old French or somewhere. Definition:

1. A person who double-crosses or otherwise does harm to a trusting party.

2. An uncommon (some even say made-up) term for a person or package which
bestows traits on (or *betrays*) a programming language.

## Overview

The *Traitor.jl* package is a naive (and potentially evil) attempt to create a
convenient framework for trait-based dispatch in the Julia programming language.
Traits provide an alternative to *single-inheritance multiple-dispatch*,
which would allow for complex combinations of object properties to be tested
at method dispatch.

Currently the package has basic functionality, supporting the features
discussed in the following sections. Some obvious oversights include lack of
support for default values and keyword arguments.

One major difference between Traitor.jl and other trait packages that utilize the 
so called "Holy trait pattern", is that Traitor.jl has it's own internal trait 
based dispatch mechanism separate from the usual Julia multiple dispatch machinery.

This difference increases the complexity of Traitor, but comes with some advantages
such as allowing multiple people who are not coordinating together to add traits to
a type and not collide with eachother. Put another way, similarly to how different 
people can create their own subtypes of an abstract type and share functions, 
Traitor allows multiple people to add traits and share trait functions.


**Warning: please have some fun using this package, but it might not yet be suitable for production code.**

### Our expectations of a traits type system

Trait types are organized into the type system. An abstract type defines the
trait class, while subtypes of them define examples of that trait.

For example:

```julia
abstract type Size end

struct Big    <: Size end
struct Medium <: Size end
struct Small  <: Size end
```

Types are annotated with traits *post-hoc* by returning the appropriate trait
*type* from the constructor of the abstract trait class given the data type in
question. So we could have:

```julia
# Note: both input and output are types
Size(::Union{Type{Int32},Type{Int64}}) = Small
Size(::Type{BigInt})                   = Big
Size(::Type{Int128})                   = Medium
```

### The `@traitor` macro

The `@traitor` macro is used to define a method which will be followed by a
further trait-based dispatch. Currently, it suffers from several limitations.
Firstly, the function must already exist (use `function f end` if necessary).
Further, `@traitor` methods are not compatible with pre-existing standard
methods, so it is best to keep in mind that `@traitor` can destructively
overwrite existing definitions. Finally, it doesn't currently support default
values or keyword arguments.

The traits to dispatch upon are given by a flexible extra set of `::` operators.
For a quick example, we allow for instance:
```julia
@traitor function howbig(x::Any::Big)
    "Huge!"
end

# The `Any` is optional
@traitor function howbig(x::::Medium)
    "So-so"
end

# One can combine standard dispatch and traits-based dispatch. 
@traitor function howbig(x::Integer::Small)
    "Teensy..."
end
```
Since standard dispatch happens before trait dispatch, the above method `howbig(x::Integer::Small)` has higher precendence than `howbig(x::::Medium)` and `howbig(x::Any::Big)`, so we need to define more specific versions of those methods:
```julia
@traitor howbig(x::Integer::Big) = "Huge!"
@traitor howbig(x::Integer::Medium) = "So-so"
```
Now,
```julia
julia> howbig(1)
"Teensy..."

julia> howbig(Int128(1))
"So-so"

julia> howbig(BigInt(1))
"Huge!"

```



### Unions of traits via `Union`

Trait constraints can be relaxed by writing `Union{}`s of trait types *of the
same trait class*. For example:
```julia
@traitor roughlyhowbig(x::::Big) = "Huge!"
@traitor roughlyhowbig(x::::Union{Medium,Small}) = "Smallish"
```

Such unions encapsulate a larger set of Julia objects than the individual traits.

### Intersection of traits via `Tuple`

Traits of *different trait classes* can be combined using a `Tuple{...}`. This
represents a *smaller* set of Julia objects - those which satisfy all of the
traits simultaneously. For example,
```julia
abstract type Odor end
struct Smelly <: Odor end

@traitor describeyourself(::::Tuple{Big,Smelly}) = "I'm big and smelly"
```

Internally to `Traitor`, the most generic trait-methods are defined by the
`Tuple{}` trait, which has no trait constraints and therefore may represent any
object.

### Computable traits

To our utter dismay, it turns out that traits can be computed. Watch out for
this slippery code below:

```julia
"The `Treason` module implements stuff that Jeff and Stefan really won't like."
module Treason
using Traitor

abstract type Mutability end
struct Immutable <: Mutability end
struct Mutable <: Mutability end

@pure Mutability(T) = T.mutable ? Mutable : Immutable

"Boring, old definition..."
@traitor function map(f, x::::Mutable)
    out = similar(x)
    for i = 1:length(x)
        out[i] = f(x[i])
    end
    return out
end

"Cool, but please optimize me onto the stack, oh Julia compiler gods"
@traitor function map(f, x::::Immutable)
    out = Ref(similar(x)) # Get one on the heap
    for i = 1:length(x) # Mutate it on the heap
        out[i] = f(x[i]) # Elsewhere define setindex!(::RefValue{Tuple}), etc
    end
    return out[] # Copy it back to the stack
end

function setindex!(x::RefValue{T}, val, i::Integer) where {T <: Tuple}
    # grab a pointer and set some memory (safely, please...)
end

end # module
```

### `betray!`ing functions

Et tu?

The `betray!` function allows one to effectively "steal"
pre-existing method definitions and make them compatible with `@traitor` methods
as a default fallback. 

```julia
module SomeoneElsesCode
f(x) = x + 1
end

import .SomeoneElsesCode: f

betray!(f, Tuple{Any}) # Inputs are provided similarly to the `methods` of `code_lowered` functions

@traitor f(x::::Big) = x - 1
```

```julia
julia> f(1)
2

julia> f(BigInt(1))
0
```

## Acknowledgements

This is joint work by Chris Foster (**@c42f**) and Andy Ferris (**@andyferris**).
We would like to thank QANTAS for providing a small space with limited distractions
for such a long time, so that we could prototype this work.
