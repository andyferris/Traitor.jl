module Traitor

import Base: @pure

export @traitor

# Uniquely reorder a Tuple type based on uid
@pure function order_types{Types <: Tuple}(::Type{Types})
    uids = map(t -> getfield(getfield(t,:name),:uid), Types.parameters)
    inds = sortperm(uids)
    Tuple{Types.parameters[inds]...}
end

# Return the trait class of a type or union.
@pure function traitclass(t)
    if typeof(t) === Union
        class = unique(map(supertype, t.types))
        if length(class) != 1
            error("unions of traits must belong to the same trait class")
        end
        return class[1]
    else
        return supertype(t)
    end
end

# Return a Tuple of trait classes reordered to the canonical ordering
@pure function traitclasses(t)
    Tuple{traitclass(t)}
end
@pure function traitclasses{T<:Tuple}(t::Type{T})
    classes = order_types(Tuple{map(traitclass, t.parameters)...})
    if length(unique(classes.parameters)) != length(classes.parameters)
        error("Repeated trait classes in $t")
    end
    classes
end

@pure ensure_Tuple(t) = Tuple{t}
@pure ensure_Tuple{T<:Tuple}(t::Type{T}) = t

@generated function construct_tuple{T<:Tuple}(::Type{T}, val)
    Expr(:tuple, [:($p(val)) for p in T.parameters]...)
end


# Extract `(normal_argument, trait)` from a function argument expression.
# `normal_argument` is the standard argument expr without the trait.  If no
# trait is present in the expression, return `nothing` for `trait`.
#
# :(x::Int::Big) -> (:(x::Int), :Big)
function extract_arg_trait(ex)
    if isa(ex, Symbol)
        return ex, nothing # argument with no type annotation
    end
    if ex.head == :kw # default values, eg f(x::T::A=1)
        arg, traittype = extract_arg_trait(ex.args[1])
        return Expr(:kw, arg, ex.args[2]), traittype
    end
    @assert ex.head == :(::)
    isfourcolon = isa(ex.args[2],Expr) && ex.args[2].head == :(::) # x::::A syntax
    if isa(ex.args[1], Symbol) && !isfourcolon
        return ex, nothing # argument with normal type annotation
    end
    if isfourcolon
        return ex.args[1], ex.args[2].args[1]
    end
    normalarg = ex.args[1]
    @assert normalarg.head == :(::)
    traittype = ex.args[2]
    return normalarg, traittype
end

#=
# :(x::T, y::S::B) ->
# ([:(x::T), :(y::S)],  [:(Tuple{}), :(Tuple{B})]
function split_traitfun_args(funcargs)
    args = Any[]
    traitargs = Any[]
    if isempty(funcargs)
        return traitargs, args
    end
    argstart = 1
    if isa(funcargs[1], Expr) && funcargs[1].head == :parameters
        # keyword arguments
        push!(args, funcargs[1])
        argstart += 1
    end
    for i = argstart:length(funcargs)
        arg, trait = extract_arg_trait(funcargs[i])
        push!(args, arg)
        push!(traitargs, trait !== nothing ? trait : )
    end
    args
end
=#

macro traitor(ex)
    if ex.head != :function
        error("trait expression must be a function")
    end
    def = ex.args[1]
    body = ex.args[2]
    @assert def.head == :call
    funcname = def.args[1]
    arg, trait = extract_arg_trait(def.args[2])
    argname = isa(arg, Symbol) ? arg : arg.args[1]
    trait = trait !== nothing ? trait : :(Tuple{})
    #newargs = split_traitfun_args(def.args[2:end])
    internalname = Symbol("_traitor_"*string(funcname))
    esc(quote
        function $funcname($arg)
            $internalname(Traitor.construct_tuple(Traitor.traitclasses($trait),$argname), $argname)
        end
        function $internalname(::Traitor.ensure_Tuple($trait), $arg)
            $(body.args...)
        end
    end)
end

end # module

