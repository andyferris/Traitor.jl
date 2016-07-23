"""
The `Traitor` package aims to bring a prototype of traits-based dispatch to
Julia. Users will primarily interact with `Traitor` via the `@traitor` macro.

Traits in `Traitor` follow a simple pattern. Firstly, all traits and groupings
of traits are types, and the traits corresponding to a Julia object can be
determined from its type alone. A "class" of traits is defined by an abstract
type, for example `abstract Size`. Examples of each trait are described by its
subtypes, for example `immutable Big <: Size; end` and
`immutable Small <: Size; end`.

Traits can be grouped in a couple of ways. Traits from the *same* class can be
combined in a `Union`, for example `Union{Big, Small}`. This union represents a
*larger* set of Julia objects than either `Big` or `Small` - they represent all
objects possessing either trait.

Conversely, traits from *different* classes can be combined in a `Tuple`, for
example `Tuple{Big, Smelly}` (where `Smelly <: Odor` is another trait from
the `Odor` class). These combinations represent a *smaller* set of Julia objects
than either `Big` or `Smelly` - they represent only objects that possess both
traits simultaneously.

To tell Julia that a type has a certain trait, you overload the constructor of
the trait class (the abstract supertype, e.g. `Size`). For example, we might
have `Size(::Type{Int}) = Small` and `Size{::Type{BigInt}} == Big`. Note that
types are used as both inputs and outputs. A default trait for all objects
can be given, e.g. `Size(::Any) = UnknownSize` (where `UnknownSize <: Size`).

Trait-based methods are created with the `@traitor` macro. Traits are defined
with an extra (optional) set of `::` in the function signature. For example:

    @traitor howbig(::Integer::Big) = "Huge!"
    @traitor howbig(::Integer::Union{Medium, Small}) = "Smallish"
    @traitor howbig(::Integer::Tuple{Big,Smelly}) = "That integer is really big and quite smelly"
    @traitor howbig(::Integer) = "Dunno"

The function is then called in the standard way, and `Traitor` will automatically
manage dispatch. If your code is statically typed, then this will be done at
compile time with no run-time overhead. For example:

    julia> howbig(1)
    "Smallish"

    julia> howbig(BigInt(1))
    "Huge!"

Finally, it is important to note that trait-dispatch occurs after standard
Julia multiple-dispatch. A function may contain many methods, of which
none, some or all will be split into trait-based submethods. The `@traitor`
macro will automatically take ownership of a single method, overwriting any
previous definition.

(see also @traitor, @betray and supertrait)
"""
module Traitor

import Base: @pure
export @traitor, @betray, supertrait


"""
    @betray f

The goal of the `@betray` macro is to take over the methods of `f` and ready
them to be compatible with traits-based dispatch, so that new submethods
defined by `@traitor` do not overwrite existing default methods (and used where
no more specific trait match is found).

WARNING: Please note this macro has NOT been implemented yet! (If anyone knows
how to turn a `Method` object into a new method definition, please let us know).

(See also `Traitor` and `@traitor`.)
"""
macro betray(ex)
    if !isa(ex, Symbol)
        error("Use @betray functionname")
    end

    return quote
        if !isdefined(ex)
            eval(:(function $ex end))
        else
            # TODO
            warn("""The @betray macro isn't currently implemented. The idea is to
                    take the methods of a generic function and put them into a
                    trait-dispatch table, so the user may then specialize these
                    methods further with traits while not losing all the
                    pre-existing definitions.""")
        end
    end
end

"""
    supertrait(trait)

For a simple trait type, returns `supertype(t)`, while for a `Union` of traits
is returns their common supertype (or else throws an error).
"""
supertrait(t::DataType) = supertype(t)
supertrait(t::Union) = supertrait_union(t)
Base.@pure function supertrait_union(t)
    traitclass = supertype(t.types[1])
    for j = 2:length(t.types)
        if traitclass != supertype(t.types[j])
            error("Unions of traits must be in the same class. Got $t")
        end
    end
    return traitclass
end
supertrait(t) = error("Unknown trait $t")


"""
    extract_arg_trait(ex) -> (normal_argument, trait)

Parse an argument signature to extract both the "normal"
typed-variable declaration and the trait expression.

If no trait is present in the expression, return `Tuple{}` for
`trait`.

# Examples

```julia
extract_arg_trait(:(x::Int::Big)) -> (:(x::Int), :(Tuple{Big}))
```
"""
function extract_arg_trait(ex)
    if isa(ex, Symbol)
        return Expr(:(::), ex, :Any), Tuple{} # argument with no type annotation
    end
    if ex.head == :parameters
        # TODO
        error("Keyword arguments are not supported by @traitor")
    end
    if ex.head == :kw # default values, eg f(x::T::A=1)
        # TODO currently these are ignored!!
        warn("Default values are currently ignored by @traitor...")
        arg, traittype = extract_arg_trait(ex.args[1])
        return Expr(:kw, arg, ex.args[2]), traittype
    end
    @assert ex.head == :(::)
    if length(ex.args) == 1 # of form ::Type, etc. We need an actual symbol name as we refer to it in the generated function
        ex.args = Any[gensym(), ex.args[1]]
    end
    isfourcolon = isa(ex.args[2],Expr) && ex.args[2].head == :(::) # x::::A syntax
    if isa(ex.args[1], Symbol) && !isfourcolon
        return ex, Tuple{} # argument with normal type annotation
    end
    if isfourcolon
        traittype = ex.args[2].args[1]
        if !isa(traittype, Expr) || traittype.head != :curly || traittype.args[1] != :Tuple
            traittype = Expr(:curly, :Tuple, traittype)
        end
        arg = (isa(ex.args[1], Symbol) ? Expr(:(::), ex.args[1], :Any) : ex.args[1])
        return arg, traittype
    end
    normalarg = ex.args[1]
    @assert normalarg.head == :(::)
    traittype = ex.args[2]
    if !isa(traittype, Expr) || traittype.head != :curly || traittype.args[1] != Tuple
        traittype = Expr(:curly, :Tuple, traittype)
    end
    return normalarg, traittype
end

"""
Each standard Julia method of a betrayed function may contain an arbitrary
number of trait-based submethods. This generated function returns (and creates,
if necessary, the first time it is called) the table joining the traits to
the submethods
"""
@generated function get_trait_table{Signature<:Tuple}(f::Function, ::Type{Signature})
    d = Dict{Any, Function}()
    return quote
        #$(Expr(:meta,:inline))
        ($d)
    end
end

"""
    @traitor f(x1::T1::Trait1, x2::T2::Trait, ...) =
    @traitor function f(x1::T1::Trait1, x2::T2::Trait, ...); ...; end

The @traitor takes over a method of function `f` to use trait-based dispatch.
If a prior non-@traitor method is defined, then that method will be deleted.
However, methods of `f` with signatures other than `f(::T1, ::T2, ...)` remain
unaffected.

Dispatch on traits occurs after normal Julian multiple-dispatch for further
specialization to sub-methods. The most specific matching trait-signature is
chosen, or an error is thrown in the case of ambiguities.

NOTE: Currently, default values and keyword arguments are not supported.

(See help for `Traitor` to learn how traits are to be used and defined.)
"""
macro traitor(ex)
    isa(ex, Expr) || error("trait expression must be a function method definition")

    # If it's a compact function definition `f(x) = y`, we convert it over to long form function `f(x); y; end`
    if ex.head == :(=) && isa(ex.args[1], Expr) && ex.args[1].head == :call
        body = ex.args[2]
        if !isa(body, Expr) || body.head != :block # Many one-liners don't have a block...
            body = Expr(:block, body)
        end
        ex = Expr(:function, ex.args[1], ex.args[2])
    end

    ex.head == :function || error("trait expression must be a function method definition")

    # Get the function name, signature and traits
    def = ex.args[1]
    body = ex.args[2]

    (isa(def, Expr) && def.head == :call) || error("trait expression must be a function method definition")
    funcname = def.args[1]

    # Go through all the arguments
    n_args = length(def.args) - 1
    args = Vector{Any}()
    argnames = Vector{Any}()
    quotednames = Vector{Any}()
    argtypes = Vector{Any}()
    traits = Vector{Any}()
    for i = 1:n_args
        arg, trait = extract_arg_trait(def.args[1+i])
        push!(args, arg)
        push!(quotednames, Expr(:quote, arg.args[1]))
        push!(argnames, arg.args[1])
        push!(argtypes, arg.args[2])
        push!(traits, trait)
    end

    argnames = (argnames...) # I'm conused why $((argnames...)) doesn't work in the quote below
    quotednames = (quotednames...)

    # Make a new name for this specialized function
    internalname = "_"*string(funcname)*"{"
    for i = 1:length(argtypes)
        internalname = internalname * "::" * string(argtypes[i]) * "::" * string(traits[i])
        if i < length(argtypes)
            internalname = internalname * ","
        end
    end
    internalname = internalname * "}"
    internalname = Symbol(internalname)

    # It's hard to get all of this right with nest quote blocks, AND it's hard
    # to get this right with Expr() objects... grrr...
    esc(Expr(:block,
        Expr(:stagedfunction, Expr(:call, funcname, args...), Expr(:block,
            :( dict = Traitor.get_trait_table($funcname, $(Expr(:curly, :Tuple, argtypes...))) ),
            :( f = Traitor.trait_dispatch(dict, $(Expr(:curly, :Tuple, argnames...))) ),
            Expr(:quote, Expr(:block,
                Expr(:meta, :inline),
                Expr(:call, Expr(:$, :f), argnames...)
            ))
        )),
        Expr(:function, Expr(:call, internalname, args...), body),
        :( d = Traitor.get_trait_table($funcname, $(Expr(:curly, :Tuple, argtypes...))) ),
        :( d[$(Expr(:curly, :Tuple, traits...))] = $internalname ),
    ))
end



"""
This function deterimines the dispatch on the traits. Each instance of
this generated function is specialized on the (standard) signature of the
inputs, so the only task remaining is to find the most specific matching
"trait signature".
"""
function trait_dispatch{Sig <: Tuple}(trait_dictionary::Dict{Any, Function}, ::Type{Sig})
    n_args = length(Sig.parameters)

    # First check which (if any) of our trait conditions is satisfied by Sig
    matching_traits = Vector{Any}()
    for traits ∈ keys(trait_dictionary)
        match = true
        # Iterate and search for lack of matches
        for n in 1:n_args
            traits_n = traits.parameters[n]

            # The traits for argument i should be a Tuple type
            for i in 1:length(traits_n.parameters)
                traitclass = supertrait(traits_n.parameters[i])

                if !(traitclass(Sig.parameters[n]) <: traits_n.parameters[i])
                    match = false
                    break
                end
            end
        end
        if match
            push!(matching_traits, traits)
        end
    end

    # Now see if there is exactly one most specific matching trait signature
    if length(matching_traits) == 0
        error("Can't find any matching trait method. Got signature $Sig and traits $(collect(keys(trait_dictionary)))")
    elseif length(matching_traits) == 1
        return trait_dictionary[matching_traits[1]]
    else # length(matching_traits) > 1
        # See if there is any matching trait that is more specific than *all*
        # other matching traits, otherwise throw an ambiguity error
        for i = 1:length(matching_traits)
            most_specific = true
            for j = 1:length(matching_traits)
                if i == j
                    continue
                end

                if is_more_specific_traitsig(matching_traits[j], matching_traits[i])
                    most_specific = false
                    break
                end
            end
            if most_specific
                return trait_dictionary[matching_traits[i]]
            end
        end
        error("Ambiguity. Could not determine most specific matching trait. Options are: $matching_traits")
    end
end

function is_more_specific_traitsig(traits, traits2)
    n = length(traits.parameters)

    more_specific = true
    for i = 1:n
        more_specific = more_specific & is_more_specific_traitarg(traits.parameters[i], traits2.parameters[i])
    end

    return more_specific
end

function is_more_specific_traitarg(traits, traits2)
    # Now arrange these into trait classes
    d = Dict{Any, Any}()
    for trait in traits.parameters
        traitclass = supertrait(trait)
        if haskey(d, traitclass)
            error("Encountered trait class $d more than once in $traits")
        else
            d[traitclass] = trait
        end
    end

    d2 = Dict{Any, Any}()
    for trait in traits2.parameters
        traitclass = supertrait(trait)
        if haskey(d2, traitclass)
            error("Encountered trait class $d more than once in $traits2")
        else
            d2[traitclass] = trait
        end
    end

    # For each trait, calculate what is going on
    more_specific = true

    for traitclass in union(keys(d), keys(d2))
        if haskey(d, traitclass)
            if haskey(d2, traitclass)
                if !(d[traitclass] <: d2[traitclass])
                    more_specific = false
                    break
                end
            end
        elseif haskey(d2, traitclass)
            # only in old trait
            more_specific = false
            break
        end
    end

    return more_specific
end

end # module
