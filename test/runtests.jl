using Traitor
using Traitor: extract_arg_trait
if VERSION < v"0.5.0-dev"
    using BaseTestNext
else
    using Base.Test
end

@testset "Parsing of function argument trait constraints" begin

# Simple trait & type symbols
@test extract_arg_trait(:(x::T::A)) == (:(x::T),   :(Tuple{A}))
@test extract_arg_trait(:(x::::A))  == (:(x::Any), :(Tuple{A}))
@test extract_arg_trait(:(x::T))    == (:(x::T),     Tuple{})
@test extract_arg_trait(:(x))       == (:(x::Any),   Tuple{})
# Type exprs
@test extract_arg_trait(:(x::Union{S,T}::A)) == (:(x::Union{S,T}), :(Tuple{A}))
@test extract_arg_trait(:(x::Union{S,T}))    == (:(x::Union{S,T}),   Tuple{})
# Trait exprs
@test extract_arg_trait(:(x::T::Union{A,B})) == (:(x::T),   :(Tuple{Union{A,B}}))
@test extract_arg_trait(:(x::::Union{A,B}))  == (:(x::Any), :(Tuple{Union{A,B}}))
# Trait & type exprs
@test extract_arg_trait(:(x::Union{S,T}::Union{A,B})) == (:(x::Union{S,T}), :(Tuple{Union{A,B}}))
@test extract_arg_trait(:(x::::Union{A,B}))  == (:(x::Any),      :(Tuple{Union{A,B}}))
@test extract_arg_trait(:(x::Union{S,T}))    == (:(x::Union{S,T}), Tuple{})
# Keyword args
@test extract_arg_trait(Expr(:kw,:(x::T::A),1)) == (Expr(:kw,:(x::T),1),   :(Tuple{A}))
@test extract_arg_trait(Expr(:kw,:(x::::A),1))  == (Expr(:kw,:(x::Any),1), :(Tuple{A}))
@test extract_arg_trait(Expr(:kw,:(x::T),1))    == (Expr(:kw,:(x::T),1),     Tuple{})
@test extract_arg_trait(Expr(:kw,:x,1))         == (Expr(:kw,:(x::Any),1),   Tuple{})

end

abstract Fooness

immutable FooA <: Fooness ; end
immutable FooB <: Fooness ; end

Fooness(::Any) = FooA()
Fooness(::Int16) = FooB()


abstract Size

immutable Big <: Size ; end
immutable Small <: Size ; end
immutable Medium <: Size ; end

Size(::Int16) = Small()
Size(::Int) = Small()
Size(::BigInt) = Big()
Size(::Int128) = Medium()

@traitor function f(x::::Big)
   "Huge"
end

@traitor function f(x::::Union{Small,Medium})
   "Smallish"
end

# FIXME!  Defining this breaks the thunk
@traitor function f(x::::Tuple{Small,FooB})
   "A small FooB"
end


@testset "@traitor" begin

@test f(1) == "Smallish"
@test f(Int128(1)) == "Smallish"
@test f(BigInt(1)) == "Huge"
#@test f(Int16(1)) == "A small FooB"

end
