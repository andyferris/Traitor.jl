module TF

import Base.@pure

immutable TraitorFunction{F <: Function}
end

TraitorFunction{F}(f::F) = TraitorFunction{F}()

# Let's rely on the fact that functions are not deleted...
#immutable TraitorMethod{F <: Function, Sig <: Tuple}
#end

#function get_arg_type(arg) # TODO: Not sure what happens if user uses :::: or ::Any::Trait forms
#    if isa(arg, Expr) && Expr.head == ::
#        if length(arg.args) == 1
#            return arg.args[1]
#        else
#            return arg.args[2]
#        end
#    end
#end

function make_name(oldname)
    newname = Symbol("_traitor_" * string(oldname))
end

macro betray(ex)
    if isa(ex, Symbol)
        # Make a mangled functor name
        fname = ex
        newname = make_name(fname)

        return quote
            # Create the function, if necessary
            if !($(Expr(:call, :isdefined, Expr(:quote, fname))))
                function $(esc(fname)) end
            end

            # Create a TraitorFunction object (if necessary)
            $(Expr(:meta, :inline))
            #if !isdefined($(esc(newname)))
            if !($(Expr(:call, :isdefined, Expr(:quote, newname))))
                $(esc(newname)) = TraitorFunction($(esc(fname)))
            end

            # Get all of the existing methods and overload them so you can
            # specialize them with trait annotations
            #$(esc(ms)) = methods()
            #$(Expr(:call, fname, args...)) = $newname(args)
        end
    else
        error("@betray acts on functions. Use `@betray func`. It will create a new function, if necessary")
    end
end
#=
macro betray(ex)
    if isa(ex, Symbol)
        error("@betray acts on function methods, not functions. Use `@betray f(::T1, ::T2, ...)`")
    elseif is(ex, Expr) && ex.head == :call
        # Get name and argument
        fname = ex.args[1]
        if length(ex.args) == 1
            args = Vector{Any}()
        else
            args = ex.args[2:end]
        end

        # Extract the signature
        argtypes = map(arg -> get_arg_type(arg), args)
        sig = Expr(:curly, :Tuple, argtypes...)

        # Make a mangled functor name
        newname = make_name(fname, argtypes)



        return quote
            $(Expr(:meta, :inline))
            $newname =
            $(Expr(:call, fname, args...)) = $newname(args)
        end
    else
        error("@betray acts on function methods, not functions. Use `@Expr f(::T1, ::T2, ...)`")
    end
end
=#

"""
Get a persisent chache of traits applied to a given function and signature.

# Dev docs

This is implemented with a generated function, which *technically* is not pure,
since it creates the dictionary
"""
@generated function get_traits{T<:Tuple}(f::Function, ::Type{T})
    d = Dict{Any, Function}()
    return quote
        $(Expr(:meta,:inline))
        ($d)
    end
end
get_traits{T}(f, t::T) = get_traits(f, T)

@pure function supertrait(t)
    if isa(t, DataType) # A single trait is a DataType
        traitclass = supertype(t)
    else # Handle unions of traits
        if !(t <: Union) || t == Union{} # because why not?
            error("Unknown trait $t")
        end

        traitclass = supertype(t.types[1])
        for j = 2:length(t.types)
            if traitclass != supertype(t.types[j])
                error("Unions of traits must be in the same class. Got $t")
            end
        end
    end
end

"""
This function organizes all of the dispatch on the traits. Each instance of
this generated function is specialized on the (standard) signature of the
inputs, so the only task remaining is to
"""
@generated function (::TraitorFunction{F}){F}(x...)
    sig = x
    f = F.instance
    trait_dictionary = get_traits(f, sig)

    # First check if our trait satisfies any trait conditions
    matching_trait = Union{}
    for trait ∈ keys(trait_dictionary)
        match = true
        # Iterate and search for lack of matches
        for i in 1:length(traits.parameters)
            n = trait.parameters[i].parameters[1]
            t = trait.parameters[i].parameters[2]

            traitclass = supertrait(t)

            if !(traitclass(x.parameters[n]) <: t)
                match = false
                break
            end

        end
        if match
            if matching_trait == Union{} # First match
                matching_trait = trait
            else
                # Check which is more specific, or report an ambiguity.
                if is_more_specific_traitsig(trait, matching_trait)
                    matching_trait = trait
                end
            end
        end
    end

    # If no trait is defined, use the original method definition
    if matching_trait == Union{}
        return quote
            $(Expr(:meta, :inline))
            $f(x...)
        end
    else
        # Otherwise look up the function dictionary and return that
        return quote
            $(Expr(:meta, :inline))
            $(trait_dictionary[matching_trait])(x...)
        end
    end
end

function is_more_specific_traitsig(traits, old_traits)
    # Maybe we could store these in (N, traitclass UID) lexicographical order?
    # It would turn into a simple iteration

    # Arrange them by argument position
    d = Dict{Int, Vector{Any}}()
    for trait in traits.parameters
        n = trait.parameters[1]
        t = trait.parameters[2]
        if haskey(d, n)
            push!(d[n], t)
        else
            d[n] = [t]
        end
    end

    d_old = Dict{Int, Vector{Any}}()
    for trait in old_traits.parameters
        n = trait.parameters[1]
        t = trait.parameters[2]
        if haskey(d_old, n)
            push!(d_old[n], t)
        else
            d_old[n] = [t]
        end
    end

    # For each position, calculate what is going on
    more_specific = false
    less_specific = false

    for n in union(keys(d), d_old)
        (m,l) = more_or_less_specific_traitarg(getkey(d, n, Vector{Any}()), getkey(d_old, n, Vector{Any}()))
        more_specific = more_specific | m
        less_specific = less_specific | l
    end

    if more_specific & less_specific
        error("Trait ambiguity. Cannot give precendence to either $traits or $old_traits.")
    elseif more_specific
        return true
    else
        return false
    end
end

function more_or_less_specific_traitarg(traits, old_traits)
    # Now arrange these into trait classes
    d = Dict{DataType, DataType}()
    for trait in traits
        traitclass = supertrait(trait)
        if haskey(d, traitclass)
            error("Encountered trait class $d more than once in $traits")
        else
            d[traitclass] = trait
        end
    end

    d_old = Dict{DataType, DataType}()
    for trait in old_traits
        traitclass = supertrait(trait)
        if haskey(d_old, traitclass)
            error("Encountered trait class $d more than once in $old_traits")
        else
            d_old[traitclass] = trait
        end
    end

    # For each position, calculate what is going on
    more_specific = false
    less_specific = false

    for traitclass in union(keys(d), d_old)
        if haskey(d, traitclass)
            if haskey(d_old, traitclass)
                more_specific = more_specific | d[traitclass]     <: d_old[traitclass]
                less_specific = less_specific | d_old[traitclass] <: d[traitclass]
            else # Only entry in new traits
                more_specific = true
            end
        elseif haskey(d_old, traitclass)
            # only in old trait
            less_specific = true
        end
    end

    return (more_specific, less_specific)
end


"""
    TraitVar{N, T}

A positional representation of a trait within a method. The trait `T` is held
to be true about argument `N`.)
"""
immutable TraitVar{N, T <: DataType}; end


end # module
