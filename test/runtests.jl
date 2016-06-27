using Traitor
using Traitor: extract_arg_trait
using BaseTestNext

@testset "Parsing of function argument trait constraints" begin

# Simple trait & type symbols
@test extract_arg_trait(:(x::T::A)) == (:(x::T), :A)
@test extract_arg_trait(:(x::::A))  == (:(x),    :A)
@test extract_arg_trait(:(x::T))    == (:(x::T), nothing)
@test extract_arg_trait(:(x))       == (:x,      nothing)
# Type exprs
@test extract_arg_trait(:(x::Union{S,T}::A)) == (:(x::Union{S,T}), :A)
@test extract_arg_trait(:(x::Union{S,T}))    == (:(x::Union{S,T}), nothing)
# Trait exprs
@test extract_arg_trait(:(x::T::Union{A,B})) == (:(x::T), :(Union{A,B}))
@test extract_arg_trait(:(x::::Union{A,B}))  == (:(x),    :(Union{A,B}))
# Trait & type exprs
@test extract_arg_trait(:(x::Union{S,T}::Union{A,B})) == (:(x::Union{S,T}), :(Union{A,B}))
@test extract_arg_trait(:(x::::Union{A,B}))  == (:(x),    :(Union{A,B}))
@test extract_arg_trait(:(x::Union{S,T}))    == (:(x::Union{S,T}), nothing)
# Keyword args
@test extract_arg_trait(Expr(:kw,:(x::T::A),1)) == (Expr(:kw,:(x::T),1), :A)
@test extract_arg_trait(Expr(:kw,:(x::::A),1))  == (Expr(:kw,:x,1),      :A)
@test extract_arg_trait(Expr(:kw,:(x::T),1))    == (Expr(:kw,:(x::T),1), nothing)
@test extract_arg_trait(Expr(:kw,:x,1))         == (Expr(:kw,:x,1),      nothing)

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
